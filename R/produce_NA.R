#' Produces a dataframe with missing values
#'
#' @param x dataframe
#' @param proportion percentage of NA on each column
#'
#' @return dataframe with missing values
#' @export

produce_NA <- function(x, proportion = 0.3){
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
