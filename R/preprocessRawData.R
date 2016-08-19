# ---- preprocess-raw-data ----
# install.packages(
#   c("dplyr",
#     "stringr",
#     "readr",
#     "magritr",
#     "assertr"
#     ))
library("dplyr")
library("stringr")
library("readr")

# ---- file-names ----
data_dir = "data"
# input file
raw_flows_file = "raw/ifr_flows_2001-2015.csv"
# output file
city_pairs_file = "ifr_city_pairs.csv"
# ---- ----

# ---- configuration ----
# year range (wef = with effect from; til = unTIL) [inclusive]
# note: changing wef (earlier) could get you into ISO code mapping nightmare
wef <- 2001
til <- 2015
# ----  ----

# explicitly mentrion na as "" because NA is a valid ISO code
# for NAMIBIA and not a null R value
# (in fact from the SQL query there are no NA's: the DEP/DES airport
# that we are not able to assign to a country are filtered out by SQL)
mydf <- read_csv(paste(data_dir,raw_flows_file, sep="/"), na = c(""))
flows <- tbl_df(mydf)
rm(mydf)


library(magrittr)

# ---- filter-years ----
# keep only flows in year [wef, til] interval
flows %<>%
  filter(YEAR >= wef & YEAR <= til)


# ---- rename-columns ----
flows %<>%
  rename(
    year = YEAR,
    flights = NB_OF_FLIGHT,
    origin_iso = DEP_ISO_COUNTRY_CODE,
    destination_iso = DES_ISO_COUNTRY_CODE)

# ---- check-invariants ----
library('assertr')
# basic checks: no NA's and right range for years
flows %<>%
  assert(not_na, destination_iso) %>%
  assert(not_na, origin_iso) %>%
  assert(in_set(seq(wef, til)), year)

# ---- fix-iso-codes ----
flows %<>%
  # **Note**: the ISO code for Serbia is provided as CS, 
  #           which was correct till 2005.
  #           Since 2006 it is RS.
  # Let's fix it:
  mutate(
    origin_iso      = ifelse(origin_iso      == "CS" & year >= 2006,
                             "RS", origin_iso),
    destination_iso = ifelse(destination_iso == "CS" & year >= 2006,
                             "RS", destination_iso)) %>%
  # Also `XF` has been used for Martinique/Guadaloupe.
  # Let's put `MQ` which will result in a proper UN region.
  mutate(
    origin_iso      = ifelse(origin_iso      == "XF", "MQ", origin_iso),
    destination_iso = ifelse(destination_iso == "XF", "MQ", destination_iso))

# ---- save-preprocessed ----
write_csv(flows, paste(data_dir, city_pairs_file, sep="/"), na="")
rm(flows)