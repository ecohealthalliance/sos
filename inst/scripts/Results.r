## ----knitr-setup, echo = FALSE-------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE)

## ----load-packages, message = FALSE, warning = FALSE---------------------
library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(devtools)

load_all()

## ----update-load-raw-data------------------------------------------------
get_current_sos()
import_country_codes()
data(sos_raw)
sosid <- paste0("SOS", 1:nrow(sos_raw))


## ----clean-data----------------------------------------------------------
name <- clean_name(sos_raw)

status <- clean_status(sos_raw)
date_created <- clean_date_created(sos_raw)
date_terminated <- clean_date_terminated(sos_raw)
countries <- clean_countries(sos_raw)


## ----survclass-calculations----------------------------------------------
humans <- clean_humans(sos_raw)
animals <- clean_animals(sos_raw)
plants <- clean_plants(sos_raw)
syndromic <- clean_syndromic(sos_raw)

sos_surveillance_class <- data.frame(sosid,
                                     humans,
                                     animals,
                                     plants,
                                     syndromic)

sos_surveillance_class %<>%
  # select(-sosid) %>%
  mutate(humans = factor(humans,
                         levels = c("yes", "no", "nf"),
                         labels = c("Yes", "No", "Not found"),
                         ordered = TRUE),
        animals = factor(animals,
                         levels = c("yes", "no", "nf"),
                         labels = c("Yes", "No", "Not found"),
                         ordered = TRUE),
        plants = factor(plants,
                         levels = c("yes", "no", "nf"),
                         labels = c("Yes", "No", "Not found"),
                         ordered = TRUE),
        # I'm removing "blank" values from the Syndromic column, because
        # I don't think the difference is useful for this table.
        syndromic = revalue(syndromic, c("blank" = "nf")),
        syndromic = factor(syndromic,
                         levels = c("yes", "no", "nf"),
                         labels = c("Yes", "No", "Not found"),
                         ordered = TRUE))

## ----entity-calculations-------------------------------------------------
entity_df <- clean_entity_type(sos_raw, return_type = "data.frame")
entity_factor <- clean_entity_type(sos_raw, return_type = "factor")

sos_entity <- data.frame(sosid, entity_df, entity_factor)


## ----survclass-table-----------------------------------------------------
knitr::kable(summary(sos_surveillance_class[2:5]),
             col.names = c("Humans", "Animals", "Plants", "Syndromic"),
             caption = "Table 1. Classes of biosurveillance conducted. Note: for \"Syndromic\" variable, \"Blank\" and \"Not Found\" entries were combined.")


## ----table-summary-------------------------------------------------------
library(reshape2)
ggsurvclass <- sos_surveillance_class %>%
  melt(id.vars = "sosid") %>%
  mutate(variable = factor(variable, labels = c("Humans", "Animals", "Plants", "Syndromic Surveillance")),
         value = factor(value, levels = c("Yes", "No", "Not found"), ordered = TRUE)) %>%
  ggplot()

ggsurvclass + geom_bar(aes(x = value, stat = "bin"), position = "dodge") + facet_grid(. ~ variable) + theme_bw() + labs(x = NULL, y = "Number of Systems", title = "Types of Biosurveillance Conducted")


## ----entity-plot---------------------------------------------------------
ggentity <- sos_entity %>%
  select(sosid, entity_factor) %>%
  ggplot()
  
ggentity + geom_bar(aes(x = entity_factor, stat = "bin"), position = "dodge") + theme_bw() + labs(x = "Classes of entity", y = "Number of Systems", title = "Types of entities conducting biosurviellance")


## ----time-series---------------------------------------------------------
sos_dates <- data.frame(date_created, date_terminated)
sos_dates$active_interval <- new_interval(sos_dates$date_created, sos_dates$date_terminated)


sum_active_systems_for_year <- function(year, intervals) {
  sum(year %within% intervals, na.rm = TRUE)
}

# Create a data frame for a time-series plot
time_series <- data.frame(year = parse_date_time(1900:2015, orders = "y"))

time_series$number_active <- sapply(time_series$year,
                                    sum_active_systems_for_year,
                                    intervals = sos_dates$active_interval)

# Create a data frame for the creation-termination histogram.
date_hist <- data.frame(sosid, select(sos_dates, date_created, date_terminated)) %>%
  melt(id.vars = "sosid") %>%
  mutate(year = year(value)) %>%
  filter(year != 2015, year >= 1950) %>%
  group_by(year, variable) %>%
  summarize(count = n()) %>%
  dcast(year ~ variable)

# We have to append rows for years not in the data frame, otherwise our over-under line will do weird things.
years_not_in_df <- seq(1950, 2014)[!seq(1950, 2014) %in% date_hist$year]
date_hist <- rbind(date_hist, data.frame(year = years_not_in_df, date_created = NA, date_terminated = NA))
date_hist[is.na(date_hist)] <- 0

max_creation <- date_hist %>%
  filter(date_created == max(date_created)) %>%
  select(year)
max_termination <- date_hist %>%
  filter(date_terminated == max(date_terminated)) %>%
  select(year)
most_systems_year <- time_series %>%
  filter(number_active == max(number_active)) %>%
  select(year) %>%
  mutate(year = year(year)) # Convert the "year" variable to a "year" from a full "date."


## ----date_hist-plot------------------------------------------------------
ggplot(data = date_hist, aes(x = year)) +
  geom_bar(aes(y = date_created), stat = "identity", fill = "#F8766D") +
  geom_bar(aes(y = -date_terminated), stat = "identity", fill = "#00BFC4") +
  geom_line(aes(y = date_created - date_terminated), size = 0.5) +
  theme_bw() +
  labs(x = "Year", y = "Number of Surveillance Systems", title = "Number of Surveillance Systems Created and Terminated")


## ----time-series-plots---------------------------------------------------
ggplot(time_series, aes(x = year, y = number_active)) + geom_line() + theme_bw() + labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time")


## ----lifespan-plot-------------------------------------------------------
active_period <- as.period(sos_dates$active_interval, unit = "years")
active_duration <- as.duration(sos_dates$active_interval)

active_years <- year(active_period)

sos_dates %<>%
  mutate(years_active = year(as.period(active_interval, unit = "years")))

xlabs <- c(seq(0, 40, by = 10), ">50")

ggplot(sos_dates, aes(x = pmin(sos_dates$years_active, 50), fill = current)) +
  layer(geom = "bar", stat = "bin", breaks = seq(0, 50)) +
  scale_x_continuous(labels = c(seq(0, 40, by = 10), ">50")) +
  labs(x = "Years Active", y = "Count", title = "Lifespan of Biosurveillance Systems") +
  theme_bw()


## ----time-series-by-entity-type------------------------------------------
sos_dates$entity_type <- clean_entity_type(sos_raw, return_type = "factor")
time_series_entity <- expand.grid(year = time_series$year, entity_type = levels(sos_dates$entity_type))
time_series_entity %<>%
  group_by(entity_type, year) %>%
  mutate(number_active = sum_active_systems_for_year(year,
                                                     sos_dates[sos_dates$entity_type == entity_type,
                                                               "active_interval"])) %>%
  ungroup() %>%
  filter(year(year) >= 1990)


ggplot(time_series_entity, aes(x = year, y = number_active, fill = entity_type, color = entity_type, order = desc(entity_type))) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Active Surveillance Systems by Entity Class")


## ----countries-----------------------------------------------------------
max_systems_in_country <- countries %>%
  summarise_each(funs = funs(sum)) %>%
  max()


## ----country-population--------------------------------------------------
library(curl)

by_country <- countries %>%
  summarise_each(funs(n = sum(as.numeric(.)))) %>%
  t() %>% # Transpose it so that countries are rows.
  as.data.frame() %>%
  mutate(iso3 = rownames(.)) %>%
  arrange(desc(V1))
names(by_country)[1] <- "n_systems"

pop_csv <- curl_download(url = "https://raw.githubusercontent.com/datasets/population/master/data/population.csv",
                         destfile = tempfile())
population <- read.csv(pop_csv, stringsAsFactors = FALSE)
population %<>%
  filter(Year == 2014) %>%
  select(iso3 = Country.Code, population = Value) 
by_country %<>%
  left_join(population) %>%
  mutate(systems_per_capita = n_systems / population)

## ----maps----------------------------------------------------------------
library(rworldmap)

sos_map <- joinCountryData2Map(dF = by_country, joinCode = "ISO3", nameJoinColumn = "iso3", verbose = FALSE)

sos_map$log_n_systems <- log(sos_map$n_systems)
spplot(sos_map, zcol = "log_n_systems", main = "Count of Surveillance Systems")

sos_map$log_systems_per_capita <- log(sos_map$systems_per_capita)
spplot(sos_map, zcol = "log_systems_per_capita", main = "Surveillance Systems Per Capita")


