# This workflow takes the column of the CSV file listing the countries, and
# converts this to a boolean matrix of ISO3 codes, with rows representing
# surveillance systems.
clean_countries <- function(countries, return_type = "data.frame", replace_countries = TRUE, ...) {
  require(plyr); require(dplyr); require(purrr)

  # This flag lets us replace some country terms which don't match in the ISO3 dataset with others that do. This has to happen at two places in the pipeline, one for things which are multi-country and need to be inserted before the splitting, and one for things which are single countries and need to be replaced after the splitting.
  if (replace_countries == TRUE) {
    single_country_replacements <- c(
      "scotland" = "united kingdom",
      "wales" = "united kingdom",
      "england" = "united kingdom",
      "south korea" = "republic of korea")
    multi_country_replacements <- c(
      "european commission" = "austria, belgium, bulgaria, croatia, republic of cyprus, czech republic, denmark, estonia, finland, france, germany, greece, hungary, ireland, italy, latvia, lithuania, luxembourg, malta, netherlands, poland, portugal, romania, slovakia, slovenia, spain, sweden, united kingdom",
      "europe" = "austria, belgium, bulgaria, croatia, republic of cyprus, czech republic, denmark, estonia, finland, france, germany, greece, hungary, ireland, italy, latvia, lithuania, luxembourg, malta, netherlands, poland, portugal, romania, slovakia, slovenia, spain, sweden, united kingdom",
      "european union" = "austria, belgium, bulgaria, croatia, republic of cyprus, czech republic, denmark, estonia, finland, france, germany, greece, hungary, ireland, italy, latvia, lithuania, luxembourg, malta, netherlands, poland, portugal, romania, slovakia, slovenia, spain, sweden, united kingdom",
      "south america" = "argentina, bolivia, brazil, chile, colombia, ecuador, guyana, paraguay, peru, suriname, uruguay, venezuela",
      "south asia" = "afghanistan, bangladesh, bhutan, india, maldives, nepal, pakistan, sri lanka"
    )

    countries_split <- countries %>%
      tolower() %>%
      revalue(multi_country_replacements, warn_missing = FALSE) %>%
      tokenize(split_and = FALSE) %>%
      map(~ revalue(.x, replace = single_country_replacements, warn_missing = FALSE))
  } else { # if replace_countries == FALSE we do the older simpler thing
    countries_split <- sos_raw$Countries %>%
      tolower() %>%
      tokenize(split_and = FALSE)
  }


  countries_list <- lapply(countries_split, match_countries)

  if (return_type == "list") {
    return(countries_list)
  } else if (return_type == "data.frame") {
    countries_matrix <- bind_rows(countries_list)
    countries_matrix[is.na(countries_matrix)] <- FALSE
    countries_matrix <- select(countries_matrix, -NA.)
    return(countries_matrix)
  }
}


# This function takes a single country name and returns the rows from the
# FAO's country_codes spreadsheet which best match the input string. If there
# are matches in both the "official name" and "short name" columns, the best
# match is returned.
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
  
  to_return <- na.omit(country_codes[best_rows, ])
  if (nrow(to_return) == 0) return(NULL)
  if (first_only == TRUE) to_return <- to_return[1, ]
  if (include_search == TRUE) to_return$search <- x
  
  return(to_return)
}


# This function is an intermediary between the `match_country()` function and
# the `clean_countries()` function. It takes a vector of country names (since
# a surveillance system can exist in multiple countries) and, for each, finds
# the best match using the `match_country()` function. It returns the
# specified variable from the FAO's country codes spreadsheet (by default,
# "iso3"), for the first match (alphabetically).
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