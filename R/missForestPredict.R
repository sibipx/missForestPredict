#' Imputes a new dataframe based on the missForest models
#'
#' @param missForestObj missForest object as returned by the missForest function
#' @param newdata new data to impute. The column names should be the same as in the imputation model
#'
#' @return an imputed dataframe
#' @export


missForestPredict <- function(missForestObj, newdata){

  # check new data columns (should be the same as in the saved object)
  if(!all(sort(names(missForestObj$init)) == sort(colnames(newdata))))
    stop("Column names for new data should be the same as in the imputation initializtion")
  if(!all(unlist(lapply(lapply(missForestObj$models, names),
                        function(x) sort(x) == sort(colnames(newdata))))))
    stop("Column names for new data should be the same as in the imputation initializtion")

  # initialize
  ximp <- newdata

  # make all integer columns double (imputed values might not be integer)
  ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)

  # TODO: give warning? test on diamonds

  for (c in names(missForestObj$init)){
    ximp[is.na(ximp[,c, drop = TRUE]),c] <- missForestObj$init[[c]]
  }

  # check that initialization is complete
  if (any(is.na(ximp))) stop("Something went wrong in initialization")

  NAloc <- is.na(newdata)
  n_iter <- length(missForestObj$models)
  if (n_iter < missForestObj$maxiter) n_iter <- n_iter - 1 # take up to the model that converged (not the last one that was worse)
  impute_sequence <- missForestObj$impute_sequence

  # impute iteratively
  if (n_iter > 0){
    for (i in 1:n_iter){
      iter_models <- missForestObj$models[[i]]

      for (c in impute_sequence){
        if (any(NAloc[,c])){ # impute only missing columns
          model <- iter_models[[c]]

          #if (class(ximp[,c]) == "factor"){
          if (is(ximp[, c, drop = TRUE], "factor")){
            # if factor, return factor
            preds <- predict(model, ximp[NAloc[,c], names(ximp)!=c])$predictions
            levels <- colnames(preds)
            preds <- apply(preds, 1, function(x) levels[which.max(x)])
            ximp[NAloc[,c],c] <- preds

          } else {

            ximp[NAloc[,c],c] <- predict(model, ximp[NAloc[,c], names(ximp)!=c])$predictions
          }
        }
      }
    }
  }

  return(ximp)

}
