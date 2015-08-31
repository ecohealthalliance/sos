library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)

load_all()

get_current_sos_()
data(sos_raw)


# Create Unique ID
sosid <- paste0("SOS", 1:nrow(sos_raw))


# Surveillance system name
name <- sos_raw$Disease.Surveillance.Database
name <- data.frame(sosid, name)


# Type of entity that created system (non-profit, for-profit, government)
entity_type <- sos_raw$NP..non.profit..FP..for.profit..Gov..Government.

entity_type %<>%
  tolower() %>%
  tokenize() %>%
  lapply(character_vector_to_logical_matrix) %>%
  rbind.fill()

entity_type[is.na(entity_type)] <- FALSE

entity_type <- cbind(sosid, entity_type)

# Character version, for good measure
entity_type_chr <- sos_raw$NP..non.profit..FP..for.profit..Gov..Government.

entity_type_chr %<>%
  tolower() %>%
  tokenize() %>%
  lapply(paste0, collapse = ", ") %>%
  unlist()

entity_type_chr <- data.frame(sosid, entity_type_chr, stringsAsFactors = FALSE)


# Active status (This first type isn't necesary because there's only one per col)
active_status <- sos_raw$Current.Terminated.Absorbed

active_status %<>%
  tolower() %>%
  tokenize() %>%
  lapply(character_vector_to_logical_matrix) %>%
  rbind.fill()

active_status[is.na(active_status)] <- FALSE

active_status <- cbind(sosid, active_status)

# And a character version, for good measure.
active_status_chr <- sos_raw$Current.Terminated.Absorbed

active_status_chr %<>%
  tolower() %>%
  tokenize() %>%
  lapply(paste0, collapse = ", ") %>%
  unlist()

active_status_chr <- data.frame(sosid, active_status_chr, stringsAsFactors = FALSE)



# Date Created
date_created <- data.frame(sosid, date_created_raw = sos_raw$Date.Created, date_created = parse_date_time(sos_raw$Date.Created, orders = c("y", "mdy")))



# Date Terminated
date_terminated_raw <- sos_raw$Date.Terminated..C.if.current.

date_terminated <- tolower(date_terminated_raw)
date_terminated[grep("c", date_terminated)] <- format(Sys.time(), "%Y")
date_terminated <- parse_date_time(date_terminated, orders = c("y", "mdy"))

date_terminated <- data.frame(sosid, date_terminated_raw, date_terminated)


# Countries

countries_raw <- sos_raw$Countries %>%
  tolower() %>%
  tokenize(split_and = FALSE)

names(countries_raw) <- sosid


# Pre-processing notes
# Change 'nf', as it matches "switzerland"

sos_countries <- list()

for (i in seq_along(countries_raw)) { # I should consider making this a function for llply.
  x <- countries_raw[[i]]
  
  if (length(x) < 1) {
    warning("Empty string for match.")
    next
  }
  
  matches <- lapply(x, match_country, first_only = TRUE, include_search = TRUE)
  matches <- do.call(rbind, matches)
  
  if (is.null(matches)) {
    warning("No matches found.")
    next
  }
  
  matches$sosid <- sosid[i]
  sos_countries[[i]] <- matches
}

# Clean up.
rm(x)
rm(i)
rm(countries_raw)
rm(matches)

sos_countries <- do.call(rbind, sos_countries)

iso3 <- sos_countries %>%
  select(sosid, iso3)

# To do: Data Type / Syndromic Surveillance / that sort of thing.
# N.B. Don't worry about "Web Activity Surveillance" as it's a mess.

# Syndromic

syndromic <- sos_raw$Syndromic.Surveillance

syndromic[grep("yes", syndromic, ignore.case = TRUE)] <- "yes"
syndromic[grep("no", syndromic, ignore.case = TRUE)] <- "no"
syndromic[grep("nf", syndromic, ignore.case = TRUE)] <- "nf"
syndromic[!syndromic %in% c("yes", "no", "nf")] <- "blank"

syndromic <- data.frame(sosid, syndromic)


# Humans

humans <- sos_raw$Humans

table(humans)

humans[grep("yes", humans, ignore.case = TRUE)] <- "yes"
humans[grep("no", humans, ignore.case = TRUE)] <- "no"
humans[grep("nf", humans, ignore.case = TRUE)] <- "nf"
humans[!humans %in% c("yes", "no", "nf")] <- "blank"

table(humans)

humans <- data.frame(sosid, humans)


# Animals

animals <- sos_raw$Animals

table(animals)

animals[grep("yes", animals, ignore.case = TRUE)] <- "yes"
animals[grep("no", animals, ignore.case = TRUE)] <- "no"
animals[grep("nf", animals, ignore.case = TRUE)] <- "nf"
animals[!animals %in% c("yes", "no", "nf")] <- "blank"

table(animals)

animals <- data.frame(sosid, animals)


# Plants

plants <- sos_raw$Plants

table(plants)

plants[grep("yes", plants, ignore.case = TRUE)] <- "yes"
plants[grep("no", plants, ignore.case = TRUE)] <- "no"
plants[grep("nf", plants, ignore.case = TRUE)] <- "nf"
plants[!plants %in% c("yes", "no", "nf")] <- "blank"

table(plants)

plants <- data.frame(sosid, plants)



to_join <- list(name, entity_type, date_created, date_terminated, active_status_chr, humans, animals, plants, syndromic)

sos_cleaned <- do.call(merge, to_join)