# install.packages("devtools")
# needs countrycode >= 0.19
# devtools::install_github("vincentarelbundock/countrycode", force = TRUE)
library(countrycode)
library(stringr)
baseurl <- 'https://raw.githubusercontent.com'
pack <- 'vincentarelbundock/countrycode'
repo <- 'master/data/extra/aviation.csv'
url <- str_c(baseurl, pack, repo, sep = '/')
library(readr)
aviation_dict = read_csv(url)
custom_dict = merge(countrycode::countrycode_data, aviation_dict)

library(dplyr)
library(stringr)
library(magrittr)
library(readr)

# ---- read-preprocessed-data ----
data_dir = "data"
city_pairs_file = "ifr_city_pairs.csv"

mydf <- read_csv(str_c(data_dir, city_pairs_file, sep = '/'), na = c(""))
flows <- tbl_df(mydf)
rm(mydf)
# ----  ----

# ---- classify-pru-regions ----
classification <- "eurocontrol_pru"
flows %<>%
  mutate(originregion_name = countrycode(origin_iso, "iso2c", classification,
                                         dictionary=custom_dict),
         destinationregion_name = countrycode(destination_iso, "iso2c", classification,
                                              dictionary=custom_dict),
         origin_name = countrycode(origin_iso, "iso2c", "country.name",
                                   dictionary=custom_dict),
         destination_name = countrycode(destination_iso, "iso2c", "country.name",
                                        dictionary=custom_dict))

# ---- fix-missing-un-region ----
flows %<>%
  mutate(originregion_name = ifelse(origin_iso == "TW",
                                    "Eastern Asia",
                                    originregion_name)) %>%
  mutate(destinationregion_name = ifelse(destination_iso == "TW",
                                         "Eastern Asia",
                                         destinationregion_name)) %>%
  
  mutate(originregion_name = ifelse(origin_iso == "IO",
                                    "Eastern Africa",
                                    originregion_name)) %>%
  mutate(destinationregion_name = ifelse(destination_iso == "IO",
                                         "Eastern Africa",
                                         destinationregion_name))


# ---- classify-flows ----
flowTypes <- c("internal", "outbound", "inbound", "overflying")
flowClass <- function(depRegion, desRegion) {
  isInternal = (depRegion == "Eurocontrol") & (desRegion == "Eurocontrol")
  if (isInternal) return("internal")
  
  isOutbound = (depRegion == "Eurocontrol") & !(desRegion == "Eurocontrol")
  if (isOutbound) return("outbound")
  
  isInbound = !(depRegion == "Eurocontrol") & (desRegion == "Eurocontrol")
  if (isInbound) return("inbound")
  
  return("overflying")
}

flows %<>% 
  mutate(ftype = 
           ifelse((originregion_name == "Eurocontrol") & 
                    (destinationregion_name == "Eurocontrol"), "internal",
                  ifelse(!(originregion_name == "Eurocontrol") &
                           (destinationregion_name == "Eurocontrol"), "inbound",
                         ifelse((originregion_name == "Eurocontrol") &
                                  !(destinationregion_name == "Eurocontrol"), "outbound",
                                "overflying"))))

#flows %<>% 
#  mutate(ftype = flowClass(originregion_name, destinationregion_name))

# ---- intra-flows ----
intra_flows <- flows %>%
  filter(ftype == "internal")

# ---- extra-flows ----
extra_flows <- flows %>%
  filter(ftype != "internal")

# ---- classify-intra-statfor ----
# split intra-Eurocontrol flights in regions according to STATFOR
classification <- "eurocontrol_statfor"
intra_flows %<>%
  mutate(originregion_name = countrycode(origin_iso, "iso2c", classification,
                                         dictionary=custom_dict),
         destinationregion_name = countrycode(destination_iso, "iso2c", classification,
                                              dictionary=custom_dict),
         origin_name = countrycode(origin_iso, "iso2c", "country.name",
                                   dictionary=custom_dict),
         destination_name = countrycode(destination_iso, "iso2c", "country.name",
                                        dictionary=custom_dict))

# ---- save-intra-flows
intra_flows_file = "intra-flows.csv"
intra_flows %>%
  write.csv(., file = paste(data_dir, intra_flows_file, sep="/"), row.names = FALSE)

# ---- extra-flow-regions-remapping ----
# Russia ->  Asia
extra_flows %<>%
  mutate(originregion_name = ifelse(originregion_name == "Russia",
                                    "Asia",
                                    originregion_name))
extra_flows %<>%
  mutate(destinationregion_name = ifelse(destinationregion_name == "Russia",
                                         "Asia",
                                         destinationregion_name))


# China ->  Asia
extra_flows %<>%
  mutate(originregion_name = ifelse(originregion_name == "China",
                                    "Asia",
                                    originregion_name))
extra_flows %<>%
  mutate(destinationregion_name = ifelse(destinationregion_name == "China",
                                         "Asia",
                                         destinationregion_name))


# Southern America ->  America
extra_flows %<>%
  mutate(originregion_name = ifelse(originregion_name == "Southern America",
                                    "America",
                                    originregion_name))

extra_flows %<>%
  mutate(destinationregion_name = ifelse(destinationregion_name == "Southern America",
                                         "America",
                                         destinationregion_name))

# Northern America -> N Ame
extra_flows %<>%
  mutate(originregion_name = ifelse(originregion_name == "Northern America",
                                    "America",
                                    originregion_name))
extra_flows %<>%
  mutate(destinationregion_name = ifelse(destinationregion_name == "Northern America",
                                         "America",
                                         destinationregion_name))

# ---- save-extra-flows
extra_flows_file = "extra-flows.csv"
extra_flows %>%
  write.csv(., file = paste(data_dir, extra_flows_file, sep="/"), row.names = FALSE)
