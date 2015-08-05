character_vector_to_logical_matrix <- function(x) {
  x <- t(as.matrix(x))
  colnames(x) <- x[1, ]
  x[1, ] <- TRUE
  mode(x) <- "logical"
  x <- as.data.frame(x)
}