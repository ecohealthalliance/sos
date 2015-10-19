

active_period <- as.period(sos_dates$active_interval, unit = "years")
active_duration <- as.duration(sos_dates$active_interval)

active_years <- year(active_period)

sos_dates %<>%
  mutate(years_active = year(as.period(active_interval, unit = "years")))

xlabs <- c(seq(0, 40, by = 10), ">50")

ggplot(sos_dates, aes(x = pmin(sos_dates$years_active, 50), fill = current)) +
  layer(geom = "bar", stat = "bin", breaks = seq(0, 50), position = "identity", alpha = 0.75) +
  scale_x_continuous(labels = c(seq(0, 40, by = 10), ">50")) +
  labs(x = "Years Active", y = "Count", title = "Lifespan of Biosurveillance Systems") +
  theme_bw()

ggsave(file = "inst/out/Lifespan of Biosurveillance Systems by current status.png", width = 6.5, height = 4.5)



ggplot(sos_dates, aes(x = pmin(sos_dates$years_active, 50), fill = current)) +
  layer(geom = geom_bar(width = 0.4, position = position_dodge(width = 4)),
        stat = stat_bin(breaks = seq(0, 50, by = 5))) +
  scale_x_continuous(labels = c(seq(0, 40, by = 10), ">50")) +
  labs(x = "Years Active", y = "Count", title = "Lifespan of Biosurveillance Systems") +
  theme_bw()





ggplot(sos_dates, aes(x = pmin(sos_dates$years_active, 50), fill = current)) +
  layer(geom = "bar",
        stat = "bin",
        width=.1,
        breaks = seq(0, 50, by = 5),
        position = position_dodge(width = 4),
        alpha = 0.9) +
  scale_x_continuous(labels = c(seq(0, 40, by = 10), ">50")) +
  labs(x = "Years Active", y = "Count", title = "Lifespan of Biosurveillance Systems") +
  theme_bw()



ggplot(sos_dates, aes(x = pmin(sos_dates$years_active, 50), fill = current)) +
  layer(geom = "bar",
        stat = "bin",
        breaks = seq(0, 50, by = 5),
        position = "identity",
        alpha = 0.5) +
  scale_x_continuous(labels = c(seq(0, 40, by = 10), ">50")) +
  labs(x = "Years Active", y = "Count", title = "Lifespan of Biosurveillance Systems") +
  theme_bw()

ggsave(file = "inst/out/Lifespan of Biosurveillance Systems by current status 5yr.png", width = 6.5, height = 4.5)



ggplot(sos_dates, aes(x = pmin(sos_dates$years_active, 50), fill = current)) +
  layer(stat = "density", breaks = seq(0, 50), position = "identity", color = "black", alpha = 0.5) +
  scale_x_continuous(labels = c(seq(0, 40, by = 10), ">50")) +
  labs(x = "Years Active", y = "Count", title = "Lifespan of Biosurveillance Systems") +
  theme_bw()

ggsave(file = "inst/out/Lifespan of Biosurveillance Systems by current status density.png", width = 6.5, height = 4.5)


ggplot(sos_dates, aes(x = pmin(sos_dates$years_active, 50), fill = current)) +
  layer(stat = "density", breaks = seq(0, 50), position = "fill", color = "black", alpha = 0.5) +
  scale_x_continuous(labels = c(seq(0, 40, by = 10), ">50")) +
  labs(x = "Years Active", y = "Count", title = "Lifespan of Biosurveillance Systems") +
  theme_bw()

