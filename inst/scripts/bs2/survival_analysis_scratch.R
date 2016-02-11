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

yn_as_logical <- function(x) {
  require(plyr)
  if (!is.logical(x)) x <- as.logical(revalue(x, replace = c(no = FALSE, yes = TRUE)))
  return(x)
}

sos <- data.frame(sosid, name, entity_type, status, date_created, date_terminated, humans, animals, plants, syndromic) %>%
  mutate(
    humans = yn_as_logical(humans),
    animals = yn_as_logical(animals),
    plants = yn_as_logical(plants),
    syndromic = yn_as_logical(syndromic)
  ) %>%
  mutate(
    year_created = year(date_created),
    year_terminated = year(date_terminated),
    time = year_terminated - year_created,
    terminated = factor(status, levels = c("current", "terminated", "absorbed")),
    terminated = revalue(terminated, replace = c("absorbed" = "terminated")),
    event = as.numeric(terminated) - 1
  )

sos_surv = Surv(sos$time, sos$event)

# Check that the "event" and "time" variables conform to the format required by Surv()
sos[1:10, c("terminated", "event", "year_created", "year_terminated", "time")]


# There are some rows where year created is not later than year terminated
sos[!(sos$year_terminated > sos$year_created), c("year_created", "year_terminated")]
 
# # We'll remove them for now.
# sos <- sos[(sos$year_terminated > sos$year_created), ]

# Create Surv object, do Kaplan-Meier plot

sos_survfit <- survfit(sos_surv ~ 1, data = sos)
ggsurv(sos_survfit)
ggsurv(sos_survfit) + xlim(0, 50)

# Look at some other breakdowns.

sos_surv_syndromic <- survfit(sos_surv ~ syndromic, data = sos)
ggsurv(sos_surv_syndromic) + xlim(0, 50)
ggsurv(sos_surv_syndromic, CI = T) + xlim(0, 50)
sos_surv_syndromic
summary(sos_surv_syndromic)

sos_surv_humans <- survfit(sos_surv ~ humans, data = sos)
ggsurv(sos_surv_humans) + xlim(0, 50)
ggsurv(sos_surv_humans, CI = T) + xlim(0, 50)
sos_surv_humans
summary(sos_surv_humans)

# Fit a cox proportional hazard model

coxph(sos_surv ~ 1)

# Univariate analyses.

m1 <- coxph(sos_surv ~ syndromic, data = sos)
summary(m1)

m2 <- coxph(sos_surv ~ humans, data = sos)
summary(m2)
m3 <- coxph(sos_surv ~ animals, data = sos)
summary(m3)
m4 <- coxph(sos_surv ~ plants, data = sos)
summary(m4)

m5 <- coxph(sos_surv ~ gov, data = sos)
summary(m5)
m6 <- coxph(sos_surv ~ np, data = sos)
summary(m6)
m7 <- coxph(sos_surv ~ fp, data = sos)
summary(m7)

# It looks like syndromic systems may just be newer than non-syndromic systems.
m8 <- coxph(sos_surv ~ year_created, data = sos) # Time is certainly important.
summary(m8)
m9 <- coxph(sos_surv ~ year_created + syndromic, data = sos)
summary(m9)
m10 <- coxph(sos_surv ~ year_created * syndromic, data = sos)
summary(m10)

# sos_cox <- coxph(formula = sos_surv ~ gov + np + fp + humans + animals + plants + syndromic, data = sos)

models <- list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)


print_summary <- function(x) {
  cat("\n")
  print(x$formula)
  # cat("\n")
  print(knitr::kable(summary(x)$coef, digits = 3))
  cat("\n")
}

#+ results = "asis"

models %>%
  map(print_summary)

#'


# x <- m10$coefficients

# coef_sum <- character()
# for (i in seq_along(x)) {
#   coef <- round(x[i], digits = 2)
#   coef_name <- names(x)[i]
#   term <- paste(coef_name, coef, sep = " = ")
#   coef_sum[i] <- term
# }
# coef_sum <- paste(coef_sum, collapse = "\n")


y1 <- sos[, c("sosid", "terminated", "year_created", "time")]
y2 <- sos[, c("sosid", "terminated", "year_terminated", "time")]

y1$what <- "start"
y2$what <- "end"

names(y1)[3] <- "year"
names(y2)[3] <- "year"

y3 <- bind_rows(y1, y2)
y3$terminated <- factor(y3$terminated, levels = rev(levels(y3$terminated)))

y3 <- na.omit(y3)

y3$time[y3$terminated == "terminated"] <- y3$time[y3$terminated == "terminated"] - 0.15
y3$time[y3$terminated == "current"] <- y3$time[y3$terminated == "current"] + 0.15

ggplot(y3) +
  geom_line(aes(x = year, y = time, color = terminated, order = -as.numeric(terminated), group = sosid),
            # position = position_jitter(width = 0),
            alpha = 0.33,
            size = 0.5) +
  theme_bw()

y3 %>%
  filter(terminated == "current", year < 2015, what == "end")

filter(sos, sosid == "SOS63") # This one says it's "current" but also terminated in 2013



# and with syndromic

y1 <- sos[, c("sosid", "terminated", "year_created", "time", "syndromic")]
y2 <- sos[, c("sosid", "terminated", "year_terminated", "time", "syndromic")]

y1$what <- "created"
y2$what <- "terminated"

names(y1)[3] <- "year"
names(y2)[3] <- "year"

y3 <- bind_rows(y1, y2)
y3$terminated <- factor(y3$terminated, levels = rev(levels(y3$terminated)))

y3 <- na.omit(y3)

y3$time[y3$terminated == "terminated"] <- y3$time[y3$terminated == "terminated"] - 0.15
y3$time[y3$terminated == "current"] <- y3$time[y3$terminated == "current"] + 0.15

ggplot(y3) +
  geom_line(aes(x = year, y = time, color = terminated, order = -as.numeric(terminated), group = sosid),
            # position = position_jitter(width = 0),
            alpha = 0.33,
            size = 0.5) +
  facet_grid(. ~ syndromic) +
  theme_bw() +
  xlim(1980, 2015) + ylim(0, 25)






# Data cleaning

# I think there are some systems listed as "current" but with termination dates in the past.

ls()

head(sos)

sos %>%
  select(contains("date"), current) %>%
  filter(current == TRUE)

names(sos_raw)

sos %>%
  filter(status == "current", current == FALSE)

# Karissa is looking into them.
# Try doing "absorbed" as "terminated"