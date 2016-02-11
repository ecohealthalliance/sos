match_country <- function(x, first_only = FALSE, include_search = FALSE) {
  
  official_rows <- grep(x, tolower(country_codes$officialname))
  short_rows <- grep(x, tolower(country_codes$shortname))
  
  rows <- list(official_rows, short_rows)
  
  score <- function(x) {
    y <- length(x)
    1/y + y
  }
  
  scores <- function(x) {
    sapply(x, score)
  }
  
  if (identical(official_rows, short_rows)) {
    best_rows <- short_rows # If they're identical and multiple, we just pick one.
  } else {
    best_rows <- rows[[which.min(scores(rows))]]
  }
  
  to_return <- country_codes[best_rows, ]
  if (nrow(to_return) == 0) return(NULL)
  if (first_only == TRUE) to_return <- to_return[1, ]
  if (include_search == TRUE) to_return$search <- x
  
  return(to_return)
}



match_countries <- function(x, return_variable = "iso3", return_all_matches = FALSE) {
  x <- x[!x %in% c("", "nf")]
  if (length(x) < 1) {
    warning("Empty string to match.")
    return(data.frame(NA))
  }

  matches <- lapply(x, match_country, first_only = TRUE, include_search = TRUE)
  matches <- do.call(rbind, matches)

  if (is.null(matches)) {
    warning("No matches found.")
    return(data.frame(NA))
  }

  if (return_all_matches == TRUE) {
    return(matches)
  }

  matches <- vector_to_matrix(matches[, return_variable])
  matches[is.na(matches)] <- FALSE

  return(matches)
}