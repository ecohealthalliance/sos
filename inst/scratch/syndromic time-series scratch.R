load_all()

date_created <- clean_date_created(sos_raw)
date_terminated <- clean_date_terminated(sos_raw)


sos_dates <- data.frame(date_created, date_terminated)
sos_dates$active_interval <- new_interval(sos_dates$date_created, sos_dates$date_terminated)


humans <- clean_humans(sos_raw)
animals <- clean_animals(sos_raw)
plants <- clean_plants(sos_raw)
syndromic <- clean_syndromic(sos_raw)

sos_surveillance_class <- data.frame(sosid,
                                     humans,
                                     animals,
                                     plants,
                                     syndromic)


sos_dates <- cbind(sos_dates, sos_surveillance_class)
sum_active_systems_for_year <- function(year, intervals) {
  sum(year %within% intervals, na.rm = TRUE)
}
time_series <- data.frame(year = parse_date_time(1900:2015, orders = "y"))
time_series_cat <- expand.grid(year = time_series$year, syndromic = levels(sos_dates$syndromic))
time_series_cat %<>%
  group_by(syndromic, year) %>%
  mutate(number_active = sum_active_systems_for_year(year,
                                                     sos_dates[sos_dates$syndromic == syndromic,
                                                               "active_interval"])) %>%
  ungroup() %>%
  filter(year(year) >= 1990,
         !syndromic == "blank")


ggplot(time_series_cat, aes(x = year, y = number_active, fill = syndromic, color = syndromic, order = desc(syndromic))) + 
  geom_line() + 
  theme_bw() +
  labs(x = "Year", y = "Count of Active Systems", title = "Active Surveillance Systems by Entity Class")
