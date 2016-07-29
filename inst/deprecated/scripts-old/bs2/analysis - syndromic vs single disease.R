load_all()

# Last run successfully 2015-10-23

library(plyr)
library(dplyr)
library(purrr)
library(magrittr)
library(lubridate)
library(ggplot2)

options(stringsAsFactors = FALSE)

get_current_sos()
data(sos_raw)

# Create Unique ID
sosid <- paste0("SOS", 1:nrow(sos_raw))

# Run individual variable cleaning functions.
name <- clean_name(sos_raw)
entity_type <- clean_entity_type(sos_raw)
status <- clean_status(sos_raw)
date_created <- clean_date_created(sos_raw)
date_terminated <- clean_date_terminated(sos_raw)
countries <- clean_countries(sos_raw)
syndromic <- clean_syndromic(sos_raw)
humans <- clean_humans(sos_raw)
animals <- clean_animals(sos_raw)
plants <- clean_plants(sos_raw)

diseases <- sos_raw$Specific.Diseases.Surveyed

# str(diseases)

# diseases %>%
#   tokenize() %>%
#   unlist() %>%
#   table() %>%
#   as.data.frame() %>%
#   arrange(desc(Freq))


# d <- diseases %>%
#   tokenize() %>%
#   lapply(vector_to_matrix) %>%
#   rbind.fill()

# diseases %>%
#   tokenize() %>%
#   as.data.frame()
#   map(length)

# This seems to be the working one
nonspecific <- grepl("nonspecific", diseases)
multiple <- diseases %>%
  tokenize() %>%
  map(length) %>%
  map(~ .x > 1) %>%
  flatten_lgl()

df <- data.frame(sosid, syndromic, nonspecific, multiple)

df$single <- !df$nonspecific & !df$multiple

df$syndromic_char <- df$syndromic
df$syndromic_fact <- factor(df$syndromic_char, levels = c("no", "yes"))
df$syndromic <- 0
df$syndromic[df$syndromic_char == "yes"] <- 1
df$syndromic[is.na(df$syndromic_fact)] <- NA
df$syndromic <- as.logical(df$syndromic)
df$syndromic_num <- as.numeric(df$syndromic)


m1 <- glm(single ~ syndromic, family = binomial(link = "logit"), data = df)
summary(m1)

print(m1$formula)

#+ results = "asis"
knitr::kable(summary(m1)$coef, digits = 3)
#'

# Syndromic vs. Year Created

df$date_created <- date_created
df$year_created <- year(date_created)

m2 <- glm(syndromic ~ year_created, family = binomial(link = "logit"), data = df)
summary(m2)

print(m2$formula)

#+ results = "asis"
knitr::kable(summary(m2)$coef, digits = 3)
#'

# Syndromic systems become significantly more prevalent over time

ggplot(na.omit(df), aes(year_created)) +
  geom_histogram(aes(fill = syndromic), position = "dodge")

ggplot(df, aes(year_created, syndromic)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df, aes(year_created, syndromic_num)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "logit"))) +
  lims(x = c(1950, 2013), y = c(0, 1))

ggplot(na.omit(df), aes(syndromic_fact, year_created)) +
  geom_violin() +
  coord_flip()

ggplot(na.omit(df), aes(year_created)) +
  geom_density(aes(fill = syndromic), position = "fill")

ggplot(na.omit(df), aes(year_created)) +
  geom_histogram(aes(fill = syndromic), position = "fill", binwidth = 2) +
  theme_bw()