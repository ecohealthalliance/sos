tokenize <- function(x, split_and = TRUE) {
  if (split_and == TRUE) {
    pattern <- "^\\s+|\\s*,\\s*|\\s*\\n\\s*|\\s*(\\band?\\b)\\s*|\\s+$"
  } else {
    pattern <- "^\\s+|\\s*,\\s*|\\s*\\n\\s*|\\s+$"
  }
  strsplit(x, split = pattern)
}


strip_whitespace <- function(x, leading = TRUE, trailing = TRUE) {
  if (leading) leading <- "^\\s+" else leading <- NULL
  if (trailing) trailing <- "\\s+$" else trailing <- NULL
  pattern <- paste(c(leading, trailing), collapse = "|")
  gsub(pattern, "", x)
}


vector_to_matrix <- function(x) {
  x <- t(as.matrix(x))
  colnames(x) <- x[1, ]
  x[1, ] <- TRUE
  mode(x) <- "logical"
  as.data.frame(x)
}