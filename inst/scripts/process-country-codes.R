load_all()

library(dplyr)

country_codes = read.csv("inst/rawdata/FAOCountryCodes.csv", stringsAsFactors = FALSE)

names(country_codes) <- c("shortname",
                      "officialname",
                      "iso3",
                      "iso2",
                      "uni",
                      "undp",
                      "faostat",
                      "gaul")

save(country_codes, file = "data/country_codes.RData")