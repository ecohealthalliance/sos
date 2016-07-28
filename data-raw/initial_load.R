load_all()

library(plyr); library(dplyr)
library(magrittr)
library(R.utils)

options(stringsAsFactors = FALSE)

# Make the current environment aware of the raw data directory. Source all
# scripts in the column cleaning workflow to load various
rawdatadir <- system.file("data-raw", package = "sos")
sourceDirectory(file.path(rawdatadir, "column_cleaning_workflows"), verbose = TRUE)

sos_raw <- read.csv("data-raw/sos_2016-07-27.csv")


#########################################
# Cleaning Surveillance System Metadata #
#########################################

# Here, we start creating the columns for the new dataset.
sosid <- paste0("sos", 1:nrow(sos_raw))

# Clean name by stripping any accidental whitespace from either end.
name <- strip_whitespace(sos_raw$name)

# Clean org_type column
org_type <- clean_org_type(sos_raw$org_type)

# Clean status column
status <- clean_status(sos_raw$status)

# Clean dates. Different functions for date_terminated and date_created, because for
# date_terminated, if the date is the year of the dataset's collection or "c", we
# replace it with the current year. Note: date_terminated is returned as a data.frame
date_created <- clean_date_created(sos_raw$date_created)
date_terminated <- clean_date_terminated(sos_raw$date_terminated)

# Clean syndromic, humans, animals, and plants columns.
humans <- clean_yes_no_neia(sos_raw$humans)
animals <- clean_yes_no_neia(sos_raw$animals)
plants <- clean_yes_no_neia(sos_raw$plants)
syndromic <- clean_yes_no_neia(sos_raw$syndromic)

sos_cleaned <- data.frame(sosid, name, org_type, status, date_created, date_terminated, humans, animals, plants, syndromic)


###########################
# Cleaning Country Column #
###########################

sos_countries <- sos_raw$countries %>%
  clean_countries() %>%
  data.frame(sosid, .)


#####################
# Saving Everything #
#####################

write.csv(sos_cleaned, file = "data-raw/sos_cleaned.csv", row.names = FALSE)
save(sos_cleaned, file = "data/sos_cleaned.RData")
write.csv(sos_countries, file = "data-raw/sos_countries.csv", row.names = FALSE)
save(sos_countries, file = "data/sos_countries.RData")
