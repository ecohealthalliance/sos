library(dplyr)

rawdatadir <- system.file("rawdata", package = "sos")

country_codes_raw <- read.table(file.path(rawdatadir, "countrynames.txt"),
                                skip = 23,
                                header = FALSE,
                                sep = ";",
                                as.is = TRUE,
                                quote = "")
# File from:
# http://www.opengeocode.org/download.php

columns <- c("ISO 3166-1 alpha-2",
             "ISO 3166-1 alpha-3",
             "ISO 3166-1 numeric",
             "ISO 3166-1 English short name (Gazetteer order)",
             "ISO 3166-1 English short name (proper reading order)",
             "ISO 3166-1 English romanized short name (Gazetteer order)",
             "ISO 3166-1 English romanized short name (proper reading oorder)",
             "ISO 3166-1 French short name (Gazetteer order)",
             "ISO 3166-1 French short name (proper reading order)",
             "ISO 3166-1 Spanish short name (Gazetteer order)",
             "UNGEGN English formal name",
             "UNGEGN French formal name",
             "UNGEGN Spanish formal name",
             "UNGEGN Russian short name",
             "UNGEGN Russian formal name",
             "UNGEGN local short name",
             "UNGEGN local formal name",
             "BGN English short name (Gazetteer order)",
             "BGN English short name (proper reading order)",
             "BGN English long name",
             "BGN local short name",
             "BGN local long name",
             "PCGN English short name (Gazetteer order)",
             "PCGN English short name (proper reading order)",
             "PCGN English long name",
             "FAO Italian long name",
             "FFO German short name")

names(country_codes_raw) <- make.names(columns)

country_names <- country_codes_raw %>%
  select(1, 2, 3, contains("English"))

save(country_names, file = "data/country_names.RData")