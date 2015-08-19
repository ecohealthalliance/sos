source("inst/scripts/1-load-and-clean.R")

library(rworldmap)

system_count <- iso3 %>%
  group_by(iso3) %>%
  summarize(num_systems = n())

system_count

system_map <- joinCountryData2Map(dF = system_count, nameJoinColumn = "iso3")

system_map$log_num_systems <- log(system_map$num_systems)

png("inst/out/demo_map_2015-08-19.png", width = 2304, height = 1440)
spplot(system_map, zcol = "num_systems")
dev.off()

png("inst/out/demo_log_map_2015-08-19.png", width = 2304, height = 1440)
spplot(system_map, zcol = "log_num_systems")
dev.off()

names(system_map)
