load_all()

library(plyr)
library(dplyr)
library(purrr)
library(magrittr)
library(lubridate)
library(survival)

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

sos <- data.frame(sosid, name, entity_type, status, date_created, date_terminated, humans, animals, plants, syndromic)

sos %<>%
  mutate(year_created = year(date_created),
         year_terminated = year(date_terminated),
         time = year_terminated - year_created,
         terminated = factor(status, levels = c("current", "terminated")))

# There are some rows where year created is not later than year terminated
sos[!(sos$year_terminated > sos$year_created), c("year_created", "year_terminated")]
 
# # We'll remove them for now.
# sos <- sos[(sos$year_terminated > sos$year_created), ]

# Create Surv object, do Kaplan-Meier plot

sos_surv <- survfit(Surv(time, as.numeric(terminated) - 1) ~ 1, data = sos)
ggsurv(sos_surv)
ggsurv(sos_surv) + xlim(0, 50)

# Look at some other breakdowns.

sos %<>%
  mutate(syndromic = factor(syndromic, levels = c("no", "yes")))

sos_surv_syndromic <- survfit(Surv(time, as.numeric(terminated)) ~ syndromic, data = sos)
ggsurv(sos_surv_syndromic) + xlim(0, 50)
ggsurv(sos_surv_syndromic, CI = T) + xlim(0, 50)
sos_surv_syndromic
summary(sos_surv_syndromic)

sos_surv_humans <- survfit(Surv(time, as.numeric(terminated)) ~ humans, data = sos)
ggsurv(sos_surv_humans) + xlim(0, 50)
ggsurv(sos_surv_humans, CI = T) + xlim(0, 50)
sos_surv_humans
summary(sos_surv_humans)

# Fit a cox proportional hazard model

sos_cox <- coxph(formula = Surv(time, as.numeric(terminated)) ~ gov + np + fp + humans + animals + plants + syndromic, data = sos)

# Transform the data

yn_as_logical <- function(x) {
  if (!is.logical(x)) x <- as.logical(revalue(x, replace = c(no = FALSE, yes = TRUE)))
  return(x)fo
}

sos %<>%
  mutate(
    humans = yn_as_logical(humans),
    animals = yn_as_logical(animals),
    plants = yn_as_logical(plants),
    syndromic = yn_as_logical(syndromic)
  )

