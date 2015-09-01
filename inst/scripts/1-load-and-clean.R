library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)

load_all()

get_current_sos()
data(sos_raw)

# Create Unique ID
sosid <- paste0("SOS", 1:nrow(sos_raw))

# Run individual variable cleaning functions.
name <- clean_name(sos_raw)
entity_type <- clean_entity_type(sos_raw)
status <- clean_status(sos_raw)
date_created <- clean_date_created(sos_raw)
date_terminated <- clean_date_terminated(sos_raw)
countries <- clean_countries(sos_raw)
syndromic <- clean_syndromic(sos_raw)
humans <- clean_humans(sos_raw)
animals <- clean_animals(sos_raw)
plants <- clean_plants(sos_raw)
