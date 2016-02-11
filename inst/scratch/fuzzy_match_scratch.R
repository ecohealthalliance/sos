library(stringdist)

dc <- daly_rate$country
cc <- country_codes$short.name

dm <- stringdistmatrix(dc, cc)


# Here, we use the minimum string distance match to pick an iso3 from the country code spreadsheet
country_codes[which.min(dm[1, ]), "iso3"]

# Here, we use the entire vector of minimum string distances
country_codes[apply(dm, 1, which.min), "iso3"]