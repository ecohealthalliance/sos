tokenize <- function(x) {
  strsplit(x, split = "^\\s+|\\s*,\\s*|\\s*(\\band?\\b)\\s*|\\s+$")
}