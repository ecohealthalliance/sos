# The option determines whether we return a dataframe/matrix with true/false
# values, or a factor of all possible combinations.
clean_org_type <- function(org_type, return_type = "data.frame") {
  require(plyr); require(dplyr)
  
  if (return_type == "data.frame") {
    org_type %<>%
      tolower() %>%
      tokenize() %>%
      lapply(., function(x) {
        if (length(x) == 0) x <- "neia"
        x[!x %in% c("fp", "np", "gov")] <- "neia" # Remove incorrect things with "neia".
        x <- unique(x) # Remove duplicates, for things like "gov, gov".
        x <- x[order(x)] # Arrange alphabetically, for things like "np, gov" and "gov, np"
      }) %>%
      lapply(vector_to_matrix) %>%
      rbind.fill()
    
    org_type[is.na(org_type)] <- FALSE
    
    return(org_type)
  } else if (return_type %in% c("character", "factor")) {
    org_type %<>%
      tolower() %>%
      tokenize() %>% # What we do here is split everything up and recombine it.
      lapply(., function(x) {
        if (length(x) == 0) x <- "neia"
        x[!x %in% c("fp", "np", "gov")] <- "neia" # Remove incorrect things with "neia".
        x <- unique(x) # Remove duplicates, for things like "gov, gov".
        x <- x[order(x)] # Arrange alphabetically, for things like "np, gov" and "gov, np"
      }) %>%
      lapply(paste0, collapse = ", ") %>%
      unlist()
    
    if (return_type == "character") return(org_type)
    
    org_levels = c("gov", 
                   "np",
                   "fp",
                   "gov, np",
                   "fp, gov",
                   "fp, np",
                   "fp, gov, np",
                   "neia")
    
    org_labels = c("Government",
                   "Non-profit", 
                   "For-profit", 
                   "Government,\nNon-profit", 
                   "Government,\nFor-profit", 
                   "Non-profit,\nFor-profit", 
                   "Government,\nNon-profit,\nFor-profit",
                   "Not found")
    
    org_type %<>% factor(levels = org_levels, labels = org_labels)
    return(org_type)
  }
}
