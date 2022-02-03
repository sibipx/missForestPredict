#' Evaluate the imputation error when true values are known.
#'
#' Evaluate the imputation error when true values are known. Useful when missing values are simulated.
#'
#' @param ximp imputed dataframe
#' @param xmis original dataframe with missing values
#' @param xtrue true dataframe with no missing values
#'
#' @return dataframe with variables in rows and performance measures in columns
#' @export


evaluate_imputation_error <- function(ximp, xmis, xtrue){

  p <- ncol(xtrue)

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
                        #BRIER = NA_real_,
                        #NMBRIER = NA_real_,
                        MER = NA_real_)

  # localize missing
  NAloc <- is.na(xmis)

  # TODO: test for tibble - DROP

  for (col in col_names){
    misi <- NAloc[,col]

    if (varType[[col]] == "numeric") {

      if (length(ximp[misi,col]) > 0) {
        results[results$variable == col, "MSE"] <- mse(ximp[misi,col, drop = TRUE], xtrue[misi,col, drop = TRUE])
        results[results$variable == col, "NMSE"] <- nmse(ximp[misi,col, drop = TRUE], xtrue[misi,col, drop = TRUE])
      } else {
        results[results$variable == col, "MSE"] <- 0
        results[results$variable == col, "NMSE"] <- 0
      }

    } else {
      if (length(ximp[misi,col]) > 0) {
        # TODO: after I return probabilities for binary variables
        # calculate BRIER
        #results["BRIER", col] <- BS(...)
        # calculate normalized BRIER

        # calculate missclassification error
        #err_MER[col] <- mer(ximp[misi,col, drop = TRUE], xtrue[misi,col, drop = TRUE])
        results[results$variable == col, "MER"] <- mer(ximp[misi,col, drop = TRUE], xtrue[misi,col, drop = TRUE])
      } else {
        results[results$variable == col, "MER"] <- 0
      }

    }

  }

  return(results)

}
