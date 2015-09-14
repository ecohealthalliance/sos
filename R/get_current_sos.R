get_current_sos <- function(save_RData = TRUE, assign = FALSE) {
  require(curl)

  sos_url <- "https://docs.google.com/spreadsheets/d/12KBoYMgqCc14WlE0JYyZLbo907f2SBPlEGgajF2j7GI/pub?gid=0&single=true&output=csv"
  date_str <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  sos_versions_path <- system.file("inst/sos_versions", package = "sos")
  rawdata_path <- system.file("inst/rawdata", package = "sos")
  data_path <- system.file("data", package = "sos")
  dest_name <- paste0("sos_", date_str, ".csv")
  dest_file <- file.path(sos_versions_path, dest_name)
  latest_file <- file.path(rawdata_path, "sos_latest.csv")

  message("Downloading current Google Drive file to compare...\n")
  sos_temp <- tryCatch(curl_download(url = sos_url, destfile = tempfile()),
                   error = function(e) {
                     simpleError("Could not download latest file.")
                   })

  if (inherits(sos_temp, "error")) {
    warning(sos_temp)
  } else if (!file.exists(latest_file)) {
    message("No cached file exists; caching current Google Drive file.\n")
    file.copy(from = sos_temp, to = latest_file, overwrite = TRUE)
    file.copy(from = sos_temp, to = dest_file, overwrite = TRUE)
  } else if (identical(read.csv(latest_file, stringsAsFactors = FALSE), read.csv(sos_temp, stringsAsFactors = FALSE))) {
    message("Latest cached file is same as Google Drive file.\n")
  } else {
    message("Google Drive file has changed from latest cached file; updating.\n")
    file.copy(from = sos_temp, to = latest_file, overwrite = TRUE) 
    file.copy(from = sos_temp, to = dest_file, overwrite = TRUE) 
  }

  sos_raw <- read.csv(latest_file, stringsAsFactors = FALSE)

  if (save_RData == TRUE) {
    message("Saving latest file as sos_raw.RData\n")
    save(sos_raw, file = file.path(data_path, "sos_raw.RData"))
  }

  if (assign == TRUE) {
    message("Assigning latest file to sos_raw object.\n")
    assign("sos_raw", sos_raw, envir = globalenv())
  }
}


