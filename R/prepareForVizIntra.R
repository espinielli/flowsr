library(stringr)
library(dplyr)
library(tidyr)
library(readr)

data_dir <- "data"
intra_flows_file = "intra-flows.csv"

mydf <- read_csv(str_c(data_dir, intra_flows_file, sep = '/'), na = c(""))
intra_flows <- tbl_df(mydf)
rm(mydf)

# ---- threshold-intra ----
# threshold for filtering out small flows
fpd <- 5 # flights per day
threshold <- 365 * fpd

# ---- create-years-file ----
intra_years <- intra_flows %>%
  select(year) %>%
  distinct() %>%
  arrange(year)

# names(years) <- c("year")

intra_years
# eventually keep only a subset of the time serie
intra_years <- intra_years[[1]] # get the array of _ALL_ year numbers
# intra_years <- c(intra_years[length(intra_years)]) # just take last one

intra_years %>%
  write.table(., sep=",", file = paste(data_dir, "intra-years-viz.csv", sep="/"),
              row.names = FALSE, col.names = c("year"), quote = TRUE)

# ---- intra-viz- thresholded ----
# keep only the years as previously defined, used to generate the list of visible countries
# CHECK: shouldn't we filter on the sum of both directions?
intra_flows %<>% 
  filter(flights >= threshold) %>%
  filter(year %in% intra_years) %>%
  spread(year, flights, fill=0)

# prepend "countryflow_"" to year columns, i.e. from "2007" to "countryflow_2007"
names(intra_flows) <- ifelse(names(intra_flows) %in% unlist(intra_years, use.names = FALSE),
                             paste("countryflow_", names(intra_flows), sep=""),
                             names(intra_flows))

intra_flows %>%
  write.csv(., file = paste(data_dir, "intra-flows-viz.csv", sep = "/"), row.names = FALSE)


# ---- create-countries-file ----
intra_descountries <- intra_flows %>%
  select(destination_iso, destination_name) %>%
  rename(iso = destination_iso, name = destination_name) %>%
  distinct()

intra_depcountries <- intra_flows %>%
  select(origin_iso, origin_name) %>%
  rename(iso = origin_iso, name = origin_name) %>%
  distinct()

intra_countries <- union(intra_descountries, intra_depcountries) %>%
  distinct() %>%
  arrange(iso) %>%
  mutate(show = 1)

# set show=1 to cous0 rows matching cous iso
# CHECK: used?
#intra_countries %<>%
#  left_join(intra_count, by="iso") %>%
#  mutate(show=show.y,name=name.x) %>%
#  select(iso,name,show) %>%
#  mutate_each(funs(replace(., which(is.na(.)), 0)))

# quotes needed due to commas in some country names
intra_countries %>%
  write.csv(., file = paste(data_dir, "intra-countries-viz.csv", sep = "/"),
            row.names = FALSE, quote = TRUE)

# ---- create-regions-file ----
# Regions in the (unfiltered) set
intra_desregions <- intra_flows %>%
  select(destinationregion_name) %>%
  rename(name = destinationregion_name) %>%
  distinct()

intra_depregions <- intra_flows %>%
  select(originregion_name) %>%
  rename(name = originregion_name) %>%
  distinct()

intra_regions <- union(intra_desregions, intra_depregions) %>%
  distinct() %>%
  arrange(name)

intra_regions %>%
  write.csv(., file = paste(data_dir, "intra-regions-viz.csv", sep="/"),
            row.names = FALSE, quote = TRUE)
