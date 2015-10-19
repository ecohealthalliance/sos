library(purrr)

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")


data(sos_raw)

syndromic_raw <- sos_raw$Syndromic.Surveillance
summary(factor(syndromic_raw))

# Old workflow
syndromic_old <- syndromic_raw

syndromic_old[grep("yes", syndromic_old, ignore.case = TRUE)] <- "yes"
syndromic_old[grep("no", syndromic_old, ignore.case = TRUE)] <- "no"
syndromic_old[grep("nf", syndromic_old, ignore.case = TRUE)] <- "nf"
syndromic_old[!syndromic_old %in% c("yes", "no", "nf")] <- "blank"


# New workflow
cleaned_value <- function(x) {
  values <- c("yes", "no", "na", "nf")
  y <- values[flatmap(values, grepl, x)] # Select a value based on grepl
  return(ifelse(length(y) >= 1, y, "no match")) # Catch empty values
}

map_chr(syndromic_raw, cleaned_value)