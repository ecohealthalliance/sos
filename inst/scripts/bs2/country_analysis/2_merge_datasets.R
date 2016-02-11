library(plyr)
library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)

load_all()

rm(list = ls())

# First: Load and merge predictor datasets. I've taken discretion to include a
# few collinear variables but will probably cull them in the next stage.

data(country_datasets)

country_data <- select(daly_rate, iso3, daly_all = all_causes, daly_comm = communicable)


country_data <- full_join(country_data,
                          select(death_rate, iso3, death_all = all_causes, death_comm = communicable))

# Here, I am counting the number of diseases per country.
disease_presence_sum <- disease_presence %>%
  select(-country, -iso3) %>%
  mutate_each(funs(as.numeric)) %>%
  mutate(diseases = rowSums(.)) %>%
  select(diseases) %>%
  cbind(disease_presence$iso3)
names(disease_presence_sum) <- c("diseases", "iso3")

country_data <- full_join(country_data, disease_presence_sum)

country_data <- full_join(country_data,
                          select(gdp_per_capita, iso3, gdp_per_cap = value))

country_data <- full_join(country_data,
                          select(health_exp, iso3, health_exp_tot_usd = total_usd, health_exp_govt_usd = govt_usd))


# Population! 

# # If https fails...
# library(curl)
# pop_csv <- curl_download(url = "https://raw.githubusercontent.com/datasets/population/master/data/population.csv", destfile = tempfile())
population <- read.csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv", stringsAsFactors = FALSE) %>%
  filter(Year == 2014) %>%
  select(iso3 = Country.Code, population = Value) 

country_data <- full_join(country_data, population)


# Surveillance systems

date_created <- clean_date_created(sos_raw)
date_terminated <- clean_date_terminated(sos_raw)
dates <- data.frame(date_created, date_terminated)
names(dates) <- c("created", "terminated", "current")
countries <- clean_countries(sos_raw)
countries_current <- countries[dates$current, ]

systems <- countries_current %>%
  summarise_each(funs(n = sum(as.numeric(.)))) %>%
  t() %>% # Transpose it so that countries are rows.
  as.data.frame() %>%
  mutate(iso3 = rownames(.)) %>%
  arrange(desc(V1))
names(systems)[1] <- "systems"

country_data <- full_join(country_data, systems)

country_data <- mutate(country_data, systems_per_cap = systems / population)

save(country_data, file = "data/country_data.RData")


# And for fun, run a test model

m1 <- lm(systems_per_cap ~ health_exp_tot_usd + gdp_per_cap + diseases + daly_all, data = country_data)

summary(m1)