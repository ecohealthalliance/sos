clean_date_created <- function(date_created) {
  require(lubridate)
  parse_date_time(date_created, orders = c("y", "mdy"))
}


clean_date_terminated <- function(date_terminated) {
  require(lubridate)
  date_terminated <- tolower(date_terminated)
  current <- grepl("c", date_terminated)
  date_terminated[grep("c", date_terminated)] <- "2015"
  date_terminated <- parse_date_time(date_terminated, orders = c("y", "mdy"))
  return(data.frame(date_terminated, current))
}