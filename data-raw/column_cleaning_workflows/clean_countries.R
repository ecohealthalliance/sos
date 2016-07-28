clean_countries <- function(sos_raw, return_type = "data.frame", replace_countries = TRUE, ...) {
  require(plyr); require(dplyr); require(purrr)

  countries_raw <- sos_raw$Countries

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

    countries_split <- countries_raw %>%
      tolower() %>%
      revalue(multi_country_replacements, warn_missing = FALSE) %>%
      tokenize(split_and = FALSE) %>%
      map(~ revalue(.x, replace = single_country_replacements, warn_missing = FALSE))
  } else { # if replace_countries == FALSE we do the older simpler thing
    countries_split <- sos_raw$Countries %>%
      tolower() %>%
      tokenize(split_and = FALSE)
  }


  countries_list <- lapply(countries_split, match_countries, ...)

  if (return_type == "list") {
    return(countries_list)
  } else if (return_type == "data.frame") {
    countries_matrix <- rbind.fill(countries_list)
    countries_matrix[is.na(countries_matrix)] <- FALSE
    countries_matrix <- select(countries_matrix, -NA.)
    return(countries_matrix)
  }
}