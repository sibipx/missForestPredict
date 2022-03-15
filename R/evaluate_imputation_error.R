#' Evaluate the imputation error when true values are known.
#'
#' Evaluate the imputation error when true values are known. Useful when missing values are simulated
#' and true values are known (the errors are calculated as distances from the true values).
#' For continuous variables, MSE (mean square error) and NMSE (normalized mean square error) are returned.
#' For categorical variables, MER (misclassification error rate) is returned.
#'
#' For details check the advanced vignette on convergence criteria and error monitoring.
#'
#'
#' @param ximp imputed dataframe.
#' @param xmis original dataframe with missing values.
#' @param xtrue true dataframe with no missing values.
#' @param all calculate error on all observations (TRUE) or only on missing observations (FALSE). Default is FALSE.
#'
#' @return Dataframe with variables in rows and performance measures in columns.
#' @export


evaluate_imputation_error <- function(ximp, xmis, xtrue, all = FALSE){

  col_names <- colnames(xtrue)
  col_names_ximp <- colnames(ximp)
  col_names_xmis <- colnames(xmis)

  if (!all(sapply(list(col_names_ximp, col_names_xmis), FUN = identical, col_names)))
    stop("Columns of the 3 dataframes should be identical")

  # check variable types
  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x) | is.character(x), "factor", NA_character_))

  varType <- unlist(lapply(xmis, column_class))

  if (any(is.na(varType))) stop("Only numeric or factor columns are supported. Logical or other types are not supported.")

  results <- data.frame(variable = col_names,
                        MSE = NA_real_,
                        NMSE = NA_real_,
                        MER = NA_real_)

  # localize missing
  NAloc <- is.na(xmis)
  ind_all <- 1:nrow(ximp)

  for (col in col_names){
    misi <- NAloc[,col]

    if (all) {
      ind <- ind_all
    } else {
      ind <- misi
    }

    if (varType[[col]] == "numeric") {

      if (length(ximp[misi, col, drop = TRUE]) > 0) {
        results[results$variable == col, "MSE"] <- mse(ximp[ind, col, drop = TRUE], xtrue[ind, col, drop = TRUE])
        results[results$variable == col, "NMSE"] <- nmse(ximp[ind, col, drop = TRUE], xtrue[ind, col, drop = TRUE])
      } else {
        results[results$variable == col, "MSE"] <- 0
        results[results$variable == col, "NMSE"] <- 0
      }

    } else {
      if (length(ximp[misi, col, drop = TRUE]) > 0) {
        results[results$variable == col, "MER"] <- mer(ximp[ind, col, drop = TRUE], xtrue[ind, col, drop = TRUE])
      } else {
        results[results$variable == col, "MER"] <- 0
      }

    }

  }

  return(results)

}
