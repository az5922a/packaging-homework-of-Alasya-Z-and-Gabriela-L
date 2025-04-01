#' Compute column means using a for-loop
#'
#' @param df A data frame with numeric columns
#' @return A numeric vector of column means
#' @export
col_means <- function(df) {
  means <- numeric(ncol(df))
  for (i in seq_along(df)) {
    means[i] <- mean(df[[i]], na.rm = TRUE)
  }
  return(means)
}

#' Count number of NA values in a vector
#'
#' @param x A vector
#' @return The number of NA values in the vector
#' @export
count_na <- function(x) {
  count <- 0
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      count <- count + 1
    }
  }
  return(count)
}