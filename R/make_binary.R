#' Gets a factor variable, returns a matrix of binary variables
#'
#' @param x a vector of type factor
#'
#' @return a matrix of binary variables

make_binary <- function(x) {

  if(!is.factor(x)) stop("A factor is expected")

  unique_vals <- levels(x)
  x_binary <- matrix(ncol = length(unique_vals), nrow = length(x))
  colnames(x_binary) <- unique_vals

  for (i in 1:length(unique_vals)) {
    x_binary[,i] <- ifelse(x == unique_vals[[i]], 1, 0)
  }

  return(x_binary)

}
