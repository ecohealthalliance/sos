## ----knitr-setup---------------------------------------------------------
# knitr::opts_chunk$set(message = FALSE)

## ----setup, message = FALSE----------------------------------------------
library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(devtools)

load_all()

# source("inst/scripts/1-load-and-clean.R", verbose = TRUE)
data(sos_raw)

sosid <- paste0("SOS", 1:nrow(sos_raw))

date_created <- clean_date_created(sos_raw)
date_terminated <- clean_date_terminated(sos_raw)

## ----create-interval-----------------------------------------------------
sos_dates <- data.frame(date_created, date_terminated)
sos_dates$active_interval <- new_interval(sos_dates$date_created, sos_dates$date_terminated)

## ----create-time-series--------------------------------------------------
sum_active_systems_for_year <- function(year, intervals) {
  sum(year %within% intervals, na.rm = TRUE)
}

time_series <- data.frame(year = parse_date_time(1900:2015, orders = "y"))

time_series$number_active <- sapply(time_series$year,
                                    sum_active_systems_for_year,
                                    intervals = sos_dates$active_interval)

# # We could also run this:
# time_series %>%
#   group_by(year) %>%
#   mutate(number_active = sum(year %within% sos_dates$active_interval, na.rm = TRUE)) %>%
#   ungroup()

## ----initial-plots-------------------------------------------------------
# Quick-and-dirty qplot.
qplot(x = year, y = number_active, data = time_series, geom = "line")

# Cleaner plot using ggplot().
ggplot(time_series, aes(x = year, y = number_active)) + geom_line() + theme_bw() + labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time")

# With number of systems on a log scale.
ggplot(time_series, aes(x = year, y = number_active)) + geom_line() + theme_bw() + labs(x = "Year", y = "Count of Active Systems (log scale)", title = "Number of Active Surveillance Systems over Time") + scale_y_log10()

## ----entity-type---------------------------------------------------------
sos_dates$entity_type <- clean_entity_type(sos_raw, return_type = "factor")

time_series_entity <- expand.grid(year = time_series$year, entity_type = levels(sos_dates$entity_type))

time_series_entity %<>%
  group_by(entity_type, year) %>%
  mutate(number_active = sum_active_systems_for_year(year,
                                                     sos_dates[sos_dates$entity_type == entity_type,
                                                               "active_interval"])) %>%
  ungroup()

## ----time-series-by-entity-type------------------------------------------
ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_area() +
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type (area)")

ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_area() +
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type (area)")

ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_area(position = "fill") +
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type (area)")


ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_area(position = "identity", alpha = 0.25) +
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type")


ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_area(position = "identity", alpha = 0.25) +
  theme_bw() +
  scale_y_log10() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type")



ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type (line)")


ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_line() + 
  theme_bw() +
  scale_y_log10() + 
  labs(x = "Year", y = "Count of Active Systems (log-transformed)", title = "Number of Active Surveillance Systems over Time by Entity Type (line)")




## ----date-created--------------------------------------------------------
library(reshape2)
qplot(year(sos_dates$date_created), binwidth = 1)
qplot(year(sos_dates$date_terminated), binwidth = 1)

date_hist <- data.frame(sosid, select(sos_dates, date_created, date_terminated)) %>%
  melt(id.vars = "sosid") %>%
  mutate(year = year(value)) %>%
  filter(year != 2015, year >= 1950)

ggplot() +
  geom_histogram(data = filter(date_hist, variable == "date_created"),
                 mapping = aes(x = year, y = ..count.., fill = "Created"), binwidth = 1) +
  geom_histogram(data = filter(date_hist, variable == "date_terminated"),
                 mapping = aes(x = year, y = -..count.., fill = "Terminated"), binwidth = 1) +
  scale_fill_hue("Group") + theme_bw() +
  labs(x = "Year", y = "Number of Surveillance Systems", title = "Number of Surveillance Systems Created and Terminated")
# ggsave(file = "inst/out/Number of Surveillance Systems Created and Terminated.pdf", width = 6.5, height = 4.5)
# ggsave(file = "inst/out/Number of Surveillance Systems Created and Terminated.png", width = 6.5, height = 4.5)

date_hist2 <- date_hist %>%
  group_by(year, variable) %>%
  summarize(count = n()) %>%
  dcast(year ~ variable)

years_not_in_df <- seq(1950, 2014)[!seq(1950, 2014) %in% date_hist2$year]

date_hist2 <- rbind(date_hist2, data.frame(year = years_not_in_df, date_created = NA, date_terminated = NA))
  
date_hist2[is.na(date_hist2)] <- 0

ggplot(data = date_hist2, aes(x = year)) +
  geom_bar(aes(y = date_created), stat = "identity", fill = "#F8766D") +
  geom_bar(aes(y = -date_terminated), stat = "identity", fill = "#00BFC4") +
  geom_line(aes(y = date_created - date_terminated), size = 0.5) +
  theme_bw() +
  labs(x = "Year", y = "Number of Surveillance Systems", title = "Number of Surveillance Systems Created and Terminated")
# ggsave(file = "inst/out/Number of Surveillance Systems Created and Terminated (with over-under line).pdf", width = 6.5, height = 4.5)
# ggsave(file = "inst/out/Number of Surveillance Systems Created and Terminated (with over-under line).png", width = 6.5, height = 4.5)



ggplot() +
  geom_histogram(data = filter(date_hist, variable == "date_created"),
                 mapping = aes(x = year, y = ..count.., fill = "Created"), binwidth = 1) +
  geom_histogram(data = filter(date_hist, variable == "date_terminated"),
                 mapping = aes(x = year, y = -..count.., fill = "Terminated"), binwidth = 1) +
  geom_line(data = time_series, mapping = aes(x = year(year), y = number_active)) +
  scale_fill_hue("Group") + theme_bw() +
  labs(x = "Year", y = "Number of Surveillance Systems", title = "Number of Surveillance Systems Created and Terminated")


