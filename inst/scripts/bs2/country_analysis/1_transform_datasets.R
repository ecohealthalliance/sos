load_all()

library(plyr)
library(dplyr)
library(purrr)
library(magrittr)

load_all()


# Load and aggregate BS database
get_current_sos()
data(sos_raw)

country_codes <- read.csv("inst/rawdata/FAOCountryCodes.csv", as.is = TRUE)
names(country_codes) <- tolower(names(country_codes))

sosid <- paste0("SOS", 1:nrow(sos_raw))
countries <- clean_countries(sos_raw)

# Aggregate countries to country level.
surveillance_systems <- colSums(countries)

# Load predictor datasets
files <- list.files("inst/rawdata/country-datasets", full.names = TRUE)
names <- c("daly_rate",
           "death_rate",
           "disease_presence",
           "health_expenditure_per_capita",
           "health_expenditure_ratio",
           "gdp_per_capita")

# Read in all files and assign to data frames
files %>%
  map(read.csv, as.is = TRUE) %>%
  walk2(.y = names, .f = ~ assign(.y, .x, envir = .GlobalEnv))
# rm(list = c("files", "names"))

#------------------------------#

as.numeric.with_commas <- (function(x) {as.numeric(gsub(",", "", x))})

## Process datasets
# DALY Rate

daly_rate <- read.csv("inst/rawdata/country-datasets/Age-standardized DALY rates.csv", skip = 1, as.is = TRUE)


daly_rate %<>%
  rename(country = Country, year = Year, all_causes = X.All.Causes, communicable = X.Communicable..amp..other.Group.I, noncommunicable = X.Noncommunicable.diseases, injuries = X.Injuries) %>%
  mutate_each(funs(as.numeric.with_commas), vars = c(-1, -2)) %>%
  filter(year == 2012) %>%
  left_join(country_codes[, c("short.name", "iso3")], by = c("country" = "short.name"))

# These didn't match for some reason
daly_rate[c(153, 163), "iso3"] <- c("MKD", "GBR")


# Death Rate
# This is processed much like the daly_rate file.

death_rate <- read.csv("inst/rawdata/country-datasets/Age-standardized death rate by three major cause groups, both sexes.csv", skip = 1, as.is = TRUE)


death_rate %<>%
  rename(country = Country, year = Year, all_causes = X.All.Causes, communicable = X.Communicable..amp..other.Group.I, noncommunicable = X.Noncommunicable.diseases, injuries = X.Injuries) %>%
  mutate_each(funs(as.numeric.with_commas), vars = c(-1, -2)) %>%
  filter(year == 2012) %>%
  left_join(country_codes[, c("short.name", "iso3")], by = c("country" = "short.name"))

# These didn't match for some reason
death_rate[c(153, 163), "iso3"] <- c("MKD", "GBR")


# Disease Presence

str(disease_presence)
disease_presence[is.na(disease_presence)] <- 0
names(disease_presence)[1] <- "country"
disease_presence <- left_join(disease_presence, country_codes[, c("short.name", "iso3")], by = c("country" = "short.name"))

disease_presence[is.na(disease_presence$iso3), "country"]

disease_presence[disease_presence$country %in% c("American Samoa", "Anguilla", "Antigua & Barbuda", "Aruba", "Bermuda", "Bolivia", "British Virgin Islands", "Brunei", "Cape Verde", "Cayman Islands", "Christmas Island", "Democratic Rep. of Congo", "East Timor", "Falkland Islands", "French Guiana", "French Polynesia", "Gibraltar", "Greenland", "Guadeloupe", "Guam", "Guinea Bissau", "Hong Kong", "Iran", "Ivory Coast", "Liechtenstein", "Macao", "Macedonia", "Martinique", "Micronesia", "Moldova", "Montserrat", "Myanmar (Burma)", "Netherlands Antilles", "New Caledonia", "Norfolk Island", "Northern Marianas", "Peoples Dem. Rep. Korea", "Pitcairn Island", "Puerto Rico", "Reunion", "Sao Tome & Principe", "Scotland", "Serbia and Montenegro", "St. Helena", "St. Kitts & Nevis", "St. Lucia", "St. Vincent & Grenadines", "Syria", "Taiwan", "Tanzania", "Tokelau", "Trinidad & Tobago", "Turks and Caicos Islands", "United States", "Venezuela", "Vietnam", "Virgin Islands, U.S.", "Wallis and Futuna Islands", "Western Sahara"), "iso3"] <- c("ASM", "AIA", "ATG", "ABW", "BMU", "BOL", "VGB", "BRN", "CPV", "CYM", "CXR", "COD", "TLS", "FLK", "GUF", "PYF", "GIB", "GRL", "GLP", "GUM", "GNB", "HKG", "IRN", "CIV", "LIE", "MAC", "MKD", "MTQ", "FSM", "MDA", "MSR", "MMR", "ANT", "NCL", "NFK", "MNP", "PRK", "PCN", "PRI", "REU", "STP", "GBR", "SRB", "SHN", "KNA", "LCA", "VCT", "SYR", "TWN", "TZA", "TKL", "TTO", "TCA", "USA", "VEN", "VNM", "VIR", "WLF", "ESH")


# Health Expenditure Per Capita

health_exp <- read.csv("inst/rawdata/country-datasets/Health expenditure per capita, all countries, selected years.csv", colClasses = "character")

health_exp <- health_exp[2:nrow(health_exp), c(1, grep(2013, health_exp[1, ]))]
names(health_exp) <- c("country", "total_usd", "total_ppp", "govt_usd", "govt_ppp")
health_exp <- mutate_each(health_exp, funs(as.numeric), vars = -1)
health_exp <- left_join(health_exp, country_codes[, c("short.name", "iso3")], by = c("country" = "short.name"))

health_exp[is.na(health_exp$iso3), "iso3"] <- c("MKD", "GBR")


# GDP Per Capita

gdp_per_capita <- read.csv("inst/rawdata/country-datasets/Per capita GDP at current prices - US dollars.csv", as.is = TRUE)

names(gdp_per_capita) <- c("country", "year", "variable", "value")

gdp_per_capita %<>%
  select(-variable) %>%
  filter(year == 2013) %>% # To be consistent with health expenditure per capita
  left_join(country_codes[, c("short.name", "iso3")], by = c("country" = "short.name")) %>%
  left_join(disease_presence[, c("country", "iso3")], by = c("country" = "country"))

gdp_per_capita[is.na(gdp_per_capita$iso3.x), "iso3.x"] <- gdp_per_capita[is.na(gdp_per_capita$iso3.x), "iso3.y"] # Replace the missing iso3s from the country code document with ones from the manually-matched disease presence document.

gdp_per_capita[is.na(gdp_per_capita$iso3.x), c("country")]

gdp_per_capita$iso3 <- gdp_per_capita$iso3.x
gdp_per_capita$iso3.x <- NULL
gdp_per_capita$iso3.y <- NULL

gdp_per_capita[gdp_per_capita$country %in% c("China, People's Republic of", "China: Hong Kong SAR", "China: Macao SAR", "Curaçao", "Iran, Islamic Republic of", "State of Palestine", "United Kingdom of Great Britain and Northern Ireland", "United Republic of Tanzania: Mainland"), "iso3"] <- c("CNH", "HKG", "MAC", "CUW", "IRN", "PSE", "GBR", "TZA")

country_datasets <- c("daly_rate", "death_rate", "disease_presence", "health_exp", "gdp_per_capita")

save(list = country_datasets, file = "data/country_datasets.RData")
