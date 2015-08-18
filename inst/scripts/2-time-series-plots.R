library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)

load_all()

source("inst/scripts/1-load-and-clean.R", verbose = TRUE)

# date1 <- parse_date_time(2001, orders = "y")
# date2 <- parse_date_time(2005, orders = "y")
# date3 <- parse_date_time(2006, orders = "y")

# int1 <- new_interval(date1, date2)

# date1 %within% int1
# date2 %within% int1
# date3 %within% int1

# Create a data frame with all date columns
sos_dates <- join_all(list(name, date_created, date_terminated), by = "sosid")
sos_dates <- select(sos_dates, sosid, name, date_created, date_terminated)

# Create a lubridate interval representing active period of system
sos_dates %<>%
  mutate(active_interval = new_interval(date_created, date_terminated))

# Okay, so if a date range has no ending, it shows as NA
sos_dates$active_interval[749]
parse_date_time(1900, "y") %within% sos_dates$active_interval[749]
parse_date_time(1971, "y") %within% sos_dates$active_interval[749]


# Because of this, we should remove all NA *start* times so we know that NAs are all NA ends.


# Test plot of count active for all systems
year <- parse_date_time(1900:2015, orders = "y")


count_active <- sapply(year, function(x) sum(x %within% sos_dates$active_interval, na.rm = TRUE))

sos_ts <- data.frame(year, count_active)

qplot(x = year, y = count_active, data = sos_ts, geom = "line")

ggplot(sos_ts, aes(x = year, y = count_active)) + geom_line() + theme_bw() + labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time")
ggsave("inst/out/active_systems.png", width = 9, height = 6.5)

ggplot(sos_ts, aes(x = year, y = count_active)) + geom_line() + theme_bw() + labs(x = "Year", y = "Count of Active Systems (log scale)", title = "Number of Active Surveillance Systems over Time") + scale_y_log10()
ggsave("inst/out/active_systems_log.png", width = 9, height = 6.5)

entity_levels = c("fp", 
                  "np", 
                  "gov", 
                  "gov, np", 
                  "gov, fp", 
                  "fp, np", 
                  "np, gov", 
                  "gov, gov", 
                  "fp, gov", 
                  "gov, np, fp")

entity_labels = c("For-profit", 
                  "Non-profit", 
                  "Government", 
                  "Government, Non-profit", 
                  "Government, For-profit", 
                  "For-profit, Non-profit", 
                  "Non-profit, Government", 
                  "Government, Government", 
                  "For-profit, Government", 
                  "Government, Non-profit, For-profit")

entity_type_fact <- entity_type_chr$entity_type_chr %>%
  factor(levels = entity_levels, labels = entity_labels) %>%
  revalue(c("Government, Government" = "Government",
            "Non-profit, Government" = "Government, Non-profit",
            "For-profit, Government" = "Government, For-profit"))


sos_ts_entity <- data.frame(sos_dates, entity_type_fact)

counts <- list()
for (i in levels(sos_ts_entity$entity_type_fact)) {
  sos_subset <- sos_ts_entity[sos_ts_entity$entity_type_fact == i, ]
  counts[[make.names(i)]] <- sapply(year, function(x) sum(x %within% sos_subset$active_interval, na.rm = TRUE))
}

sos_ts_ent <- data.frame(year, counts)

library(reshape2)

sos_ts_ent <- melt(sos_ts_ent, id.vars = "year")
levels(sos_ts_ent$variable) <- levels(entity_type_fact)

ggplot(sos_ts_ent, aes(x = year, y = value, fill = variable, color = variable, order = desc(variable))) + 
  geom_area() +
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type (area)")
ggsave("inst/out/active_systems_by_entity_type_area_stacked.png", width = 9, height = 6.5)


ggplot(sos_ts_ent, aes(x = year, y = value, fill = variable, color = variable, order = desc(variable))) + 
  geom_area(position = "identity", alpha = 0.25) +
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type")
ggsave("inst/out/active_systems_by_entity_type_area_not_stacked.png", width = 9, height = 6.5)


ggplot(sos_ts_ent, aes(x = year, y = value, fill = variable, color = variable, order = desc(variable))) + 
  geom_area(position = "identity", alpha = 0.25) +
  theme_bw() +
  scale_y_log10() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type")
ggsave("inst/out/active_systems_by_entity_type_area_not_stacked_log.png", width = 9, height = 6.5)



ggplot(sos_ts_ent, aes(x = year, y = value, fill = variable, color = variable, order = desc(variable))) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Number of Active Surveillance Systems over Time by Entity Type (line)")
ggsave("inst/out/active_systems_by_entity_type_line.png", width = 9, height = 6.5)


ggplot(sos_ts_ent, aes(x = year, y = value, fill = variable, color = variable, order = desc(variable))) + 
  geom_line() + 
  theme_bw() +
  scale_y_log10() + 
  labs(x = "Year", y = "Count of Active Systems (log-transformed)", title = "Number of Active Surveillance Systems over Time by Entity Type (line)")
ggsave("inst/out/active_systems_by_entity_type_line_log.png", width = 9, height = 6.5)

sink("inst/out/entity_type.txt")
table(entity_type_fact)
sink()

qplot(entity_type_fact, fill = entity_type_fact) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position="none") + labs(x = "Entity Type", y = "Count", title = "Count of Surveillance Systems by Entity Type")
ggsave("inst/out/entity_type_histogram.png", width = 9, height = 6.5)