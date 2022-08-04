#' Creates the default predictor matrix with 0 diagonal and 1 elements in the rest.
#'
#' Creates a square predictor matrix with number of rows and columns equal to the number of columns of the data.
#' Each row name represents the name of the column to be imputed. Each column name represents a predictor for the imputation model.
#' A row will therefore contain 1 for the columns corresponding to variable names that should be included
#' as predictors in the imputation model for the variable with the corresponding row name
#' and 0 if the column should not be included.
#' The diagonal of the predictor matrix is 0, indicating that variable X will not be a predictor for the
#' imputation model of variable X.
#' All other values are 1, meaning that all other variables will be included in the imputation model.
#' This is the default predictor matrix used.
#'
#' @param data dataframe to be imputed
#'
#' @return predictor matrix that can be used as a start for setting a custom predictor matrix
#' @export

create_predictor_matrix <- function(data) {

  predictor_matrix <- matrix(1, nrow = ncol(data), ncol = ncol(data))
  dimnames(predictor_matrix) <- list(colnames(data), colnames(data))
  diag(predictor_matrix) <- 0

  return(predictor_matrix)
}
