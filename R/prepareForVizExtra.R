library(tidyr)
library(readr)

data_dir <- "data"
extra_flows_file = "extra-flows.csv"

mydf <- read_csv(str_c(data_dir, extra_flows_file, sep = '/'), na = c(""))
extra_flows <- tbl_df(mydf)
rm(mydf)


# threshold for filtering out small flows (3650 = flow of 10 flights/day between dep/des country)
threshold <- 3650 / 2

# ---- create-years-file ----
extra_years <- extra_flows %>%
  select(year) %>%
  distinct() %>%
  arrange(year)

# names(years) <- c("year")

extra_years
# eventually keep only a subset of the time serie
extra_years <- extra_years[[1]] # get the array of _ALL_ year numbers
# extra_years <- c(extra_years[length(extra_years)]) # just take last one

extra_years %>%
  write.table(., sep=",", file = paste(data_dir, "extra-years-viz.csv", sep="/"),
              row.names = FALSE, col.names = c("year"), quote = TRUE)

# ---- extra-viz-thresholded ----
# keep only the years as previously defined, used to generate the list of visible countries
# CHECK: shouldn't we filter on the sum of both directions?
extra_flows %<>% 
  filter(flights > threshold) %>%
  filter(year %in% extra_years) %>%
  spread(year, flights, fill=0)

# prepend "countryflow_"" to year columns, i.e. from "2007" to "countryflow_2007"
names(extra_flows) <- ifelse(names(extra_flows) %in% unlist(extra_years, use.names = FALSE),
                             paste("countryflow_", names(extra_flows), sep=""),
                             names(extra_flows))

extra_flows %>%
  write.csv(., file = paste(data_dir, "extra-flows-viz.csv", sep = "/"), row.names = FALSE)


# ---- create-countries-file ----
extra_descountries <- extra_flows %>%
  select(destination_iso, destination_name) %>%
  rename(iso = destination_iso, name = destination_name) %>%
  distinct()

extra_depcountries <- extra_flows %>%
  select(origin_iso, origin_name) %>%
  rename(iso = origin_iso, name = origin_name) %>%
  distinct()

extra_countries <- union(extra_descountries, extra_depcountries) %>%
  distinct() %>%
  arrange(iso) %>%
  mutate(show = 1)

# set show=1 to cous0 rows matching cous iso
# CHECK: used?
#extra_countries %<>%
#  left_join(extra_count, by="iso") %>%
#  mutate(show=show.y,name=name.x) %>%
#  select(iso,name,show) %>%
#  mutate_each(funs(replace(., which(is.na(.)), 0)))

# quotes needed due to commas in some country names
extra_countries %>%
  write.csv(., file = paste(data_dir, "extra-countries-viz.csv", sep = "/"),
            row.names = FALSE, quote = TRUE)

# ---- create-regions-file ----
# Regions in the (unfiltered) set
extra_desregions <- extra_flows %>%
  select(destinationregion_name) %>%
  rename(name = destinationregion_name) %>%
  distinct()

extra_depregions <- extra_flows %>%
  select(originregion_name) %>%
  rename(name = originregion_name) %>%
  distinct()

extra_regions <- union(extra_desregions, extra_depregions) %>%
  distinct() %>%
  arrange(name)

extra_regions %>%
  write.csv(., file = paste(data_dir, "extra-regions-viz.csv", sep="/"),
            row.names = FALSE, quote = TRUE)
