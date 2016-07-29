clean_status <- function(status) {
  library(plyr); library(dplyr); library(magrittr)

  status %<>%
    tolower() %>%
    strip_whitespace() %>%
    tokenize() %>%
    lapply(paste0, collapse = ", ") %>%
    unlist()

  status[!status %in% c("absorbed", "current", "terminated", "neia")] <- NA

  return(status)
}