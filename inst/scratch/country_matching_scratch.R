library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)

load_all()

get_current_sos()

names(sos)


# Create Unique ID
sosid <- paste0("SOS", 1:nrow(sos))






# Countries
countries <- sos$Countries

countries_unlist <- countries %<>%
  tolower() %>%
  tokenize(split_and = FALSE) %>%
  unlist() 

table(countries_unlist)[order(table(countries_unlist), decreasing = TRUE)]
# There's an EU entry. The EU countries are: Austria, Belgium, Bulgaria, Croatia, Republic of Cyprus, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, Netherlands, Poland, Portugal, Romania, Slovakia, Slovenia, Spain, Sweden and the UK.

countries_unique <- unique(countries_unlist)

data(country_codes)

# These are the countries that match more than one "officialname":
lapply(countries_unique, grep, tolower(country_codes$officialname)) %>%
  sapply(length) %>%
  sapply(`>`, 1) %>%
  countries_unique[.]

# However, I don't think this solves all of our problems.
# For instance, "uk" won't match "United Kingdom; it'll match "Ukraine".

# These are the countries that match zero "officialname" entries:
lapply(countries_unique, grep, tolower(country_codes$officialname)) %>%
  sapply(length) %>%
  sapply(`==`, 0) %>%
  countries_unique[.]


# How the same thing for the shortname: countries matching >1
lapply(countries_unique, grep, tolower(country_codes$shortname)) %>%
  sapply(length) %>%
  sapply(`>`, 1) %>%
  countries_unique[.]

# Countries matching 0 shortnames:
lapply(countries_unique, grep, tolower(country_codes$officialname)) %>%
  sapply(length) %>%
  sapply(`==`, 0) %>%
  countries_unique[.]




officialname_matches <- lapply(countries_unique, grep, tolower(country_codes$officialname), value = TRUE)
names(officialname_matches) <- countries_unique
shortname_matches <- lapply(countries_unique, grep, tolower(country_codes$shortname), value = TRUE)
names(shortname_matches) <- countries_unique

officialname_indices <- lapply(countries_unique, grep, tolower(country_codes$officialname))
lapply(officialname_indices, function(x, countries) {
  if (length(x) > 1) x <- x[1]
  country_codes[x, ]
}, countries = country_codes)



match_country <- function(x, first_only = FALSE, include_search = FALSE) {
  
  official_rows <- grep(x, tolower(country_codes$officialname))
  short_rows <- grep(x, tolower(country_codes$shortname))
  
  rows <- list(official_rows, short_rows)
  
  score <- function(x) {
    as.numeric(dist(c(1, length(x))))
  }
  
  scores <- function(x) {
    sapply(x, score)
  }
  
  
  if (identical(official_rows, short_rows)) {
    best_rows <- short_rows # If they're identical and multiple, we just pick one.
  } else {
    best_rows <- rows[[which.min(scores(rows))]]
  }
  
  to_return <- country_codes[best_rows, ]
  if (nrow(to_return) == 0) return(NULL)
  if (first_only == TRUE) to_return <- to_return[1, ]
  if (include_search == TRUE) to_return$search <- x
  
  return(to_return)
}

test <- lapply(countries_unique, match_country)
names(test) <- countries_unique
lapply(test, nrow) # There are still some countries which have two matches.



countries <- sos$Countries %>%
  tolower() %>%
  tokenize(split_and = FALSE)

names(countries) <- sosid


# Pre-processing notes
# Change 'nf', as it matches "switzerland"

sos_country_codes <- list()

i <- 659
i <- 687
i <- 759
i <- 750
i <- 5

for (i in seq_along(countries)) {
  x <- countries[[i]]
  
  if (length(x) < 1) {
    warning("Empty string for match.")
    next
  }
  
  countries_matched <- lapply(x, match_country, first_only = TRUE, include_search = TRUE)
  countries_matched <- do.call(rbind, countries_matched)
  
  if (is.null(countries_matched)) {
    warning("No matches found.")
    next
  }
  
  countries_matched$sosid <- sosid[i]
  sos_country_codes[[i]] <- countries_matched
}

sos_country_codes <- do.call(rbind, sos_country_codes)

# Here's how we'll process countries:
# 1. We'll conduct some pre-processing, for countries we know won't match properly.
# 2. We'll match every country with its shortname and officialname
# 3. We'll convert that into a data frame.




