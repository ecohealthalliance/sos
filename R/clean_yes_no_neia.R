clean_yes_no_neia <- function(x) {
  x[grep("yes", x, ignore.case = TRUE)] <- "yes"
  x[grep("no", x, ignore.case = TRUE)] <- "no"
  x[grep("neia", x, ignore.case = TRUE)] <- "neia"
  x[!x %in% c("yes", "no", "neia")] <- "blank"

  return(x)
}