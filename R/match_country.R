match_country <- function(x, first_only = FALSE, include_search = FALSE) {
  
  official_rows <- grep(x, tolower(country_codes$officialname))
  short_rows <- grep(x, tolower(country_codes$shortname))
  
  rows <- list(official_rows, short_rows)
  
  score <- function(x) {
    as.numeric(dist(c(1, length(x))))
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