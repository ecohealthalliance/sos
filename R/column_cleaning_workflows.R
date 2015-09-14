clean_name <- function(sos_raw) {
  name <- sos_raw$Disease.Surveillance.Database
  name <- strip_whitespace(name)
  return(name)
}

# The option determines whether we return a dataframe/matrix with true/false
# values, or a factor of all possible combinations.
clean_entity_type <- function(sos_raw, return_type = "data.frame") {
  require(plyr); require(dplyr)
  
  entity_type <- sos_raw$NP..non.profit..FP..for.profit..Gov..Government.  
  
  if (return_type == "data.frame") {
    entity_type %<>%
      tolower() %>%
      tokenize() %>%
      lapply(., function(x) {
        if (length(x) == 0) x <- "nf"
        x[!x %in% c("fp", "np", "gov")] <- "nf" # Remove incorrect things with "nf".
        x <- unique(x) # Remove duplicates, for things like "gov, gov".
        x <- x[order(x)] # Arrange alphabetically, for things like "np, gov" and "gov, np"
      }) %>%
      lapply(vector_to_matrix) %>%
      rbind.fill()
    
    entity_type[is.na(entity_type)] <- FALSE
    
    return(entity_type)
  } else if (return_type %in% c("character", "factor")) {
    entity_type %<>%
      tolower() %>%
      tokenize() %>% # What we do here is split everything up and recombine it.
      lapply(., function(x) {
        if (length(x) == 0) x <- "nf"
        x[!x %in% c("fp", "np", "gov")] <- "nf" # Remove incorrect things with "nf".
        x <- unique(x) # Remove duplicates, for things like "gov, gov".
        x <- x[order(x)] # Arrange alphabetically, for things like "np, gov" and "gov, np"
      }) %>%
      lapply(paste0, collapse = ", ") %>%
      unlist()
    
    if (return_type == "character") return(entity_type)
    
    entity_levels = c("gov", 
                      "np",
                      "fp",
                      "gov, np",
                      "fp, gov",
                      "fp, np",
                      "fp, gov, np",
                      "nf")
    
    entity_labels = c("Government",
                      "Non-profit", 
                      "For-profit", 
                      "Government,\nNon-profit", 
                      "Government,\nFor-profit", 
                      "Non-profit,\nFor-profit", 
                      "Government,\nNon-profit,\nFor-profit",
                      "Not found")
    
    entity_type %<>% factor(levels = entity_levels, labels = entity_labels)
    return(entity_type)
  }
}


clean_status <- function(sos_raw) {
  status <- sos_raw$Current.Terminated.Absorbed

  status %<>%
    tolower() %>%
    tokenize() %>%
    lapply(paste0, collapse = ", ") %>%
    unlist()

  status[!status %in% c("absorbed", "current", "nf", "terminated")] <- NA

  return(status)
}


clean_date_created <- function(sos_raw) {
  require(lubridate)
  parse_date_time(sos_raw$Date.Created, orders = c("y", "mdy"))
}


clean_date_terminated <- function(sos_raw) {
  require(lubridate)
  date_terminated <- tolower(sos_raw$Date.Terminated..C.if.current.)
  date_terminated[grep("c", date_terminated)] <- format(Sys.time(), "%Y")
  parse_date_time(date_terminated, orders = c("y", "mdy"))
}




clean_countries <- function(sos_raw, return_type = "data.frame", ...) {
  require(plyr); require(dplyr)

  countries_raw <- sos_raw$Countries %>%
    tolower() %>%
    tokenize(split_and = FALSE)

  match_countries <- function(x, return_variable = "iso3") {
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

    matches <- vector_to_matrix(matches[, return_variable])
    matches[is.na(matches)] <- FALSE

    return(matches)
  }

  countries_list <- lapply(countries_raw, match_countries)

  if (return_type == "list") {
    return(countries_list)
  } else if (return_type == "data.frame") {
    countries_matrix <- rbind.fill(countries_list)
    countries_matrix[is.na(countries_matrix)] <- FALSE
    countries_matrix <- select(countries_matrix, -NA.)
    return(countries_matrix)
  }
}




clean_syndromic <- function(sos_raw) {
  syndromic <- sos_raw$Syndromic.Surveillance

  syndromic[grep("yes", syndromic, ignore.case = TRUE)] <- "yes"
  syndromic[grep("no", syndromic, ignore.case = TRUE)] <- "no"
  syndromic[grep("nf", syndromic, ignore.case = TRUE)] <- "nf"
  syndromic[!syndromic %in% c("yes", "no", "nf")] <- "blank"

  return(syndromic)
}



clean_humans <- function(sos_raw) {
  humans <- sos_raw$Humans

  humans[grep("yes", humans, ignore.case = TRUE)] <- "yes"
  humans[grep("no", humans, ignore.case = TRUE)] <- "no"
  humans[grep("nf", humans, ignore.case = TRUE)] <- "nf"
  humans[!humans %in% c("yes", "no", "nf")] <- "blank"

  return(humans)
}



clean_animals <- function(sos_raw) {
  animals <- sos_raw$Animals

  animals[grep("yes", animals, ignore.case = TRUE)] <- "yes"
  animals[grep("no", animals, ignore.case = TRUE)] <- "no"
  animals[grep("nf", animals, ignore.case = TRUE)] <- "nf"
  animals[!animals %in% c("yes", "no", "nf")] <- "blank"

  return(animals)
}



clean_plants <- function(sos_raw) {
  plants <- sos_raw$Plants

  plants[grep("yes", plants, ignore.case = TRUE)] <- "yes"
  plants[grep("no", plants, ignore.case = TRUE)] <- "no"
  plants[grep("nf", plants, ignore.case = TRUE)] <- "nf"
  plants[!plants %in% c("yes", "no", "nf")] <- "blank"

  return(plants)
}
