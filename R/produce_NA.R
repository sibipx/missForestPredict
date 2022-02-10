#' Produces a dataframe with missing values
#'
#' Produces a dataframe with missing values.
#' For each variable, a random sample of observations of size \code{proportion * nrow(x)} will be set to NA.
#'
#' @param x a dataframe
#' @param proportion proportion of missing values to be produced; a vector of size \code{ncol(x)} or a single value to be applied to all variables in the dataframe
#'
#' @return dataframe with missing values
#' @export

produce_NA <- function(x, proportion = 0.1){
  n <- nrow(x)
  p <- ncol(x)

  if (length(proportion) > 1 & length(proportion) != p)
    stop(sprintf("Either a scalar or a vector of size %s is expected (number of columns in x)", p))

  if (length(proportion) == 1) proportion <- rep(proportion, p)

  for (i in 1:p){
    number_NA <- floor(n * proportion[[i]])
    if (number_NA > 0) {
      x[sample(n, number_NA), i] <- NA
    }
  }

  return(x)
}
