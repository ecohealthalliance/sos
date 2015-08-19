tokenize <- function(x, split_and= TRUE) {
  if (split_and== TRUE) {
    pattern = "^\\s+|\\s*,\\s*|\\s*(\\band?\\b)\\s*|\\s+$"
  } else {
    pattern = "^\\s+|\\s*,\\s*|\\s+$"
  }
  strsplit(x, split = pattern)
}