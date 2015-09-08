import_country_codes <- function(csv_file = "inst/rawdata/FAOCountryCodes.csv") {
  csv_file <- system.file(csv_file, package = "sos")
  data_path <- system.file("data", package = "sos")
  
  country_codes = read.csv(csv_file, stringsAsFactors = FALSE)
  names(country_codes) <- c("shortname",
                            "officialname",
                            "iso3",
                            "iso2",
                            "uni",
                            "undp",
                            "faostat",
                            "gaul")
  save(country_codes, file = file.path(data_path, "country_codes.RData"))
}
