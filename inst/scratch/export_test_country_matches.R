# This script is designed to export a CSV file with two columns. The first is the verbatim raw countries list from the Google Spreadsheet, and the second the names of countries matched from the spreadsheet.

load_all()

library(plyr)
library(dplyr)

countries_raw <- sos_raw$Countries

countries_split <- sos_raw$Countries %>%
  tolower() %>%
  tokenize(split_and = FALSE)

countries_list <- lapply(countries_split, match_countries, return_all_matches = TRUE)

for (i in 1:length(countries_list)) {
  try(countries_list[[i]] <- data.frame(i, countries_list[[i]][, c("search", "shortname")]))
}

countries_df <- rbind.fill(countries_list)

# This doesn't include things which didn't match anything.
write.csv(countries_df, file = "inst/out/countries_matches.csv")

# To do:
# - print list of things which didn't match
# - Write code which substitutes continent names


# Now we want to look for non-matches.

# Get raw entries where the country match is NA
non_matches <- countries_raw[unlist(map(countries_list, ~ all(is.na(.x))))]

non_matches %>%
  tokenize() %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  arrange(-Freq) -> unique_non_matches

unique_non_matches
#                      . Freq
# 1             Scotland   42
# 2       European Union   33
# 3                Wales   19
# 4              England   17
# 5                  N/A    9
# 6                  n/a    7
# 7                   nf    5
# 8                   NF    5
# 9               Europe    4
# 10         South Korea    4
# 11              Taiwan    2
# 12      United Nations    2
# 13 European Commission    1
# 14       South America    1
# 15          South Asia    1
# 16     Southern Africa    1

# Things to replace




# New try




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

countries_list <- lapply(countries_split, match_countries, return_all_matches = TRUE)

for (i in 1:length(countries_list)) {
  try(countries_list[[i]] <- data.frame(i, countries_list[[i]][, c("search", "shortname")]))
}

countries_df <- rbind.fill(countries_list)

# This doesn't include things which didn't match anything.
write.csv(countries_df, file = "inst/out/countries_matches.csv")

# To do:
# - print list of things which didn't match
# - Write code which substitutes continent names


# Now we want to look for non-matches.

# Get raw entries where the country match is NA
non_matches <- countries_raw[unlist(map(countries_list, ~ all(is.na(.x))))]

non_matches %>%
  tokenize() %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  arrange(-Freq) -> unique_non_matches

unique_non_matches


# There are still some non-matches
x <- as.data.frame(unlist(map(countries_list, ~ all(is.na(.x)))))
x$i <- 1:nrow(x)
names(x)[1] <- "unmatched"
x %<>% filter(unmatched == TRUE)

countries_raw[[163]]
countries_split[[163]]
countries_list[[163]] # Here's the bad step.
x <- countries_list[[163]]
match_countries(countries_split[[163]])