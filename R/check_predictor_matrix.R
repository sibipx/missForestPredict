#' Performs checks on a custom predictor matrix
#'
#' Performs a number of checks on a custom predictor matrix and returns error if the matrix is
#' not in a valid format.
#'
#' @param predictor_matrix custom predictor matrix
#' @param data dataframe to be imputed
#' @param verbose if TRUE, returns human readable message for the checks performed.
#'
#' @export

check_predictor_matrix <- function(predictor_matrix, data, verbose = TRUE) {

  p <- ncol(data)
  col_names <- colnames(data)

  if (!is.matrix(predictor_matrix)) {
    stop("predictor_matrix should be a matrix.")
  }

  if (nrow(predictor_matrix) != p | ncol(predictor_matrix) != p)
    stop("predictor_matrix needs to be a square matrix with the number of rows and columns equal to the number of columns of the dataframe to be imputed.")

  if (any(sort(rownames(predictor_matrix)) != sort(col_names)) |
      any(sort(colnames(predictor_matrix)) != sort(col_names)))
    stop("The row names and column names of predictor_matrix need to be the same as the column names of the dataframe.")

  if (!is.numeric(predictor_matrix) | sum(!predictor_matrix %in% c(0,1)) != 0)
    stop("predictor_matrix should be numeric with elements 0 or 1. NA values are also not allowed")

  if(sum(diag(predictor_matrix)) != 0)
    stop("The diagonal of predictor matrix should be 0. You can set it to 0 using `diag(predictor_matrix) <- 0`")

  if(all(predictor_matrix == 0))
    stop("All values in the predictor matrix are 0. At least one element should be 1.")

  if (verbose){
    message("GREAT! All checks passed succesfully.")
    default_predictor_matrix <- create_predictor_matrix(data)

    if (all(predictor_matrix == default_predictor_matrix))
      message("The predictor matrix is the same as the default predictor matrix. All variables will be used as predictors.")

    is_not_imputed <- rowSums(predictor_matrix) == 0
    names_is_not_imputed <- names(is_not_imputed)[is_not_imputed]

    if (any(is_not_imputed))
      message(sprintf("Variable(s): %s will not be imputed",
                      paste(names_is_not_imputed, collapse = ", ")))

    vars_to_impute <- rownames(predictor_matrix)[!rownames(predictor_matrix) %in% names_is_not_imputed]

    for (v in vars_to_impute){

      predictors_used <- predictor_matrix[v, ]
      if (sum(predictors_used) == p - 1){
        predictor_string <- "all other variables"
      } else {
        predictor_string <- paste(names(predictors_used)[predictors_used == 1], collapse = ", ")
      }

      message(sprintf("Variable %s will be imputed using %s.", v, predictor_string))
    }

  }

}
