#' Imputes a new dataframe based on the missForest models
#'
#' Imputes a new dataframe based on the missForest models. The same number of iterations as in missForest are used.
#'
#' @param missForestObj missForest object as returned by the missForest function
#' @param newdata new data to impute. The column names should be the same as in the imputation model.
#' @param x_init initialization dataframe in case custom initialization mode has been used
#'
#' @return an imputed dataframe
#' @examples
#' data(iris)
#' # split train / test and create missing values
#' id_test <- sample(1:nrow(iris), floor(nrow(iris)/3))
#'
#' iris_train <- iris[-id_test,]
#' iris_test <- iris[id_test,]
#'
#' iris_train_miss <- produce_NA(iris_train, proportion = 0.1)
#' iris_test_miss <- produce_NA(iris_test, proportion = 0.1)
#'
#' # impute train and learn imputation models
#' iris_train_imp_obj <- missForest(iris_train_miss)
#'
#' # impute test
#' iris_test_imp_new <- missForestPredict(iris_train_imp_obj, newdata = iris_test_miss)
#' head(iris_test_imp_new)
#' @export


missForestPredict <- function(missForestObj, newdata, x_init = NULL){

  # checks for custom
  if (missForestObj$initialization == "custom"){
    if(is.null(x_init))
      stop("missForest was run with custom initialization. x_init should not be NULL.")
    if(any(any(dim(newdata) != dim(x_init))))
      stop(sprintf("x_init needs to have the same dimensions as newdata: %s", paste(dim(newdata), collapse = ", ")))
    if(sum(!complete.cases(x_init)) > 0)
      stop("x_init needs to be a complete dataframe with no missing values")
  } else { # check for non-custom
    if(!length(missForestObj$init) == ncol(newdata))
      stop("Column names for new data should be the same as in the imputation initialization")
    if(!all(sort(names(missForestObj$init)) == sort(colnames(newdata))))
      stop("Column names for new data should be the same as in the imputation initialization")
    if(!all(unlist(lapply(lapply(missForestObj$models, names),
                          function(x) sort(x) == sort(colnames(newdata))))))
      stop("Column names for new data should be the same as in the imputation initialization")
  }

  # initialize
  if (missForestObj$initialization != "custom") {
    ximp <- newdata
    # make all integer columns double (imputed values might not be integer)
    ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)

    for (c in names(missForestObj$init)){
      ximp[is.na(ximp[,c, drop = TRUE]),c] <- missForestObj$init[[c]]
    }
  } else {
    ximp <- x_init
    # make all integer columns double (imputed values might not be integer)
    ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)
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
          if (is(ximp[, c, drop = TRUE], "factor") | is(ximp[, c, drop = TRUE], "character")){
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
