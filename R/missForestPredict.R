#' Imputes a new dataframe based on the missForest models
#'
#' @param missForestObj missForest object as returned by the missForest function
#' @param newdata new data to impute. The column names should be the same as in the imputation model
#'
#' @return an imputed dataframe

missForestPredict <- function(missForestObj, newdata){

  # check new data columns (should be the same as in the saved object)
  if(!all(sort(names(data_small_imp_new$init)) == sort(colnames(newdata))))
    stop("Column names for new data should be the same as in the imputation initializtion")
  if(!all(unlist(lapply(lapply(data_small_imp_new$models, names),
                        function(x) sort(x) == sort(colnames(newdata))))))
    stop("Column names for new data should be the same as in the imputation initializtion")

  # initialize
  ximp <- newdata
  for (c in names(missForestObj$init)){
    ximp[is.na(ximp[,c]),c] <- missForestObj$init[[c]]
  }

  # check that initialization is complete
  if (any(is.na(ximp))) stop("Something went wrong in initialization")


  # for iteration
    # for column
      # impute

  return(ximp)

}
