# Symptoms and Diseases

symptoms <- sos$Specific.Symptoms.Surveyed

diseases <- sos$Specific.Diseases.Surveyed

diseases_unlist <- diseases %>%
  tolower() %>%
  tokenize(split_and = FALSE) %>%
  unlist()

# Some fields are split on new lines (\n).
# Some parenthetical statements.
# Some "all" stated in various ways

grep("influenza", diseases, ignore.case = TRUE, value = TRUE) #77
grep("flu", diseases, ignore.case = TRUE, value = TRUE) # 80

grep("hiv", diseases, ignore.case = TRUE, value = TRUE)
grep("aids", diseases, ignore.case = TRUE, value = TRUE)

grep("mers", diseases, ignore.case = TRUE, value = TRUE)

grep("malaria", diseases, ignore.case = TRUE, value = TRUE)

grep("all", diseases, ignore.case = TRUE, value = TRUE)
grep("all notifiable", diseases, ignore.case = TRUE, value = TRUE)



grep("influenza", diseases_unlist, ignore.case = TRUE, value = TRUE) #77
grep("flu", diseases_unlist, ignore.case = TRUE, value = TRUE) # 80

grep("hiv", diseases_unlist, ignore.case = TRUE, value = TRUE)
grep("aids", diseases_unlist, ignore.case = TRUE, value = TRUE)

grep("mers", diseases_unlist, ignore.case = TRUE, value = TRUE)

grep("malaria", diseases_unlist, ignore.case = TRUE, value = TRUE)

grep("all", diseases_unlist, ignore.case = TRUE, value = TRUE)
grep("all notifiable", diseases_unlist, ignore.case = TRUE, value = TRUE)

