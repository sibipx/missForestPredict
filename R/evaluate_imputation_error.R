#' Title
#'
#' @param ximp imputed matrix / dataframe
#' @param xmis original matrix / dataframe with missing values
#' @param xtrue true matrix / dataframe (or any matrix to be compared with ximp)
#' @param variableWise logical; should the errors be returned for each variable (TRUE) or aggregated by variable type (numeric / factor)
#'
#' @return named vector of errors
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
                                     ifelse(is.factor(x), "factor", NA_character_))

  varType <- unlist(lapply(xmis, column_class))

  if (any(is.na(varType))) stop("Only numeric or factor columns are supported. Logical or other types are not supported.")

  # initialize error lists per variable
  err_MSE <- rep(NA, p)
  names(err_MSE) <- col_names
  err_NMSE <- rep(NA, p)
  names(err_NMSE) <- col_names
  err_BRIER <- rep(NA, p)
  names(err_BRIER) <- col_names
  err_NBRIER <- rep(NA, p)
  names(err_NBRIER) <- col_names
  err_MER <- rep(NA, p)
  names(err_MER) <- col_names

  # localize missing
  NAloc <- is.na(xmis)

  # TODO: test for tibble - DROP

  for (col in col_names){
    misi <- NAloc[,col]

    if (varType[[col]] == "numeric") {

      if (length(ximp[misi,col]) > 0) {
        # calculate MSE
        err_MSE[col] <- mse(ximp[misi,col], xtrue[misi,col])
        # calculate NMSE
        err_NMSE[col] <- nmse(ximp[misi,col], xtrue[misi,col])
      } else {
        err_MSE[col] <- 0
        err_NMSE[col] <- 0
      }

    } else {
      # calculate BRIER

      # calculate normalized BRIER

      # calculate missclassification error
      err_MER[col] <- mer(ximp[misi,col], xtrue[misi,col])

    }

  }

  return(list(MSE = err_MSE, NMSE = err_NMSE, MER = err_MER))

}
