#' Title
#'
#' @param x matrix / dataframe
#' @param noNA percentage of NA on each column
#'
#' @return matrix / dataframe with missing values
#' @export


prodNA <- function(x, noNA = 0.1){
  n <- nrow(x)
  p <- ncol(x)
  NAloc <- rep(FALSE, n*p)
  NAloc[sample(n * p, floor(n * p * noNA))] <- TRUE
  x[matrix(NAloc, nrow = n, ncol = p)] <- NA
  return(x)
}
