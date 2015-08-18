get_current_sos <- function(assign = TRUE) {
  require(curl)

  stringsAsFactors.init <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

  sos_url <- "https://docs.google.com/spreadsheets/d/12KBoYMgqCc14WlE0JYyZLbo907f2SBPlEGgajF2j7GI/pub?gid=0&single=true&output=csv"
  date_str <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  sos_versions_path <- system.file("inst/sos_versions", package = "sos")
  rawdata_path <- system.file("inst/rawdata", package = "sos")
  dest_name <- paste0("sos_", date_str, ".csv")
  dest_file <- file.path(sos_versions_path, dest_name)
  latest_file <- file.path(rawdata_path, "sos_latest.csv")
  sos_temp <- tempfile()

  if (!file.exists(latest_file)) {
    cat("No cached file exists; caching current Google Drive file.\n")
    curl_download(url = sos_url, destfile = latest_file)
    file.copy(from = latest_file, to = dest_file, overwrite = TRUE)
  } else {
    curl_download(url = sos_url, destfile = sos_temp)
    cat("Downloading current Google Drive file to compare...\n")
    if (identical(read.csv(latest_file), read.csv(sos_temp))) {
      cat("Latest cached file is same as Google Drive file.\n")
    } else {
      cat("Google Drive file has changed from latest cached file; updating.\n")
      file.copy(from = sos_temp, to = latest_file, overwrite = TRUE) 
      file.copy(from = sos_temp, to = dest_file, overwrite = TRUE) 
    }
  }


  if (assign == TRUE) assign("sos", read.csv(latest_file), envir = parent.frame())
  options(stringsAsFactors = stringsAsFactors.init)
}