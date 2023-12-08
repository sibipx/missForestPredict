#' Imputes a new dataframe based on the missForest models
#'
#' Imputes a new dataframe based on the missForest models. The same number of iterations as in missForest are used.
#'
#' A new observation is initialized in the same manner as passed through the \code{initialization}
#' parameter to the \code{missForest} function. Then, variables are imputed in the same sequence and for the same
#' number of iterations using the random models saved for each iteration. This ensures that a new observation is
#' imputed in the same manner as the training set (imputed by the function \code{missForest}).
#' Re-imputing the training set with the \code{missForestPredict} will yield the same result as
#' the original imputation returned by the \code{missForest} function.
#'
#' @param missForestObj missForest object as returned by the missForest function.
#' @param newdata new data to impute. The column names should be the same as in the imputation model.
#' @param x_init initialization dataframe in case custom initialization mode has been used.
#' It needs to be complete dataframe (with no missing values). See vignette for a full example.
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
#' iris_train_imp_obj <- missForest(iris_train_miss, save_models = TRUE, num.threads = 2)
#'
#' # impute test
#' iris_test_imp_new <- missForestPredict(iris_train_imp_obj, newdata = iris_test_miss)
#' head(iris_test_imp_new)
#'
#' @import ranger
#' @importFrom stats predict
#' @importFrom methods is
#' @importFrom stats complete.cases
#' @export


missForestPredict <- function(missForestObj, newdata, x_init = NULL){

  # check if models were saved
  if (!missForestObj$save_models)
    stop("The parameter save_models has been set to FALSE when running missForest.
         missForestPredict can only run when save_models = TRUE")

  # get variables from predictor_matrix (should never be null)
  predictor_matrix <- missForestObj$predictor_matrix
  vars_included_to_impute <- rownames(predictor_matrix)[!rowSums(predictor_matrix) == 0]
  vars_included_as_pred <- colnames(predictor_matrix)[!colSums(predictor_matrix) == 0]
  vars_used <- unique(c(vars_included_to_impute, vars_included_as_pred))

  # checks for custom
  if (missForestObj$initialization == "custom"){
    if(is.null(x_init))
      stop("missForest was run with custom initialization. x_init should not be NULL.")
    if(any(any(dim(newdata) != dim(x_init))))
      stop(sprintf("x_init needs to have the same dimensions as newdata: %s", paste(dim(newdata), collapse = ", ")))
    if(sum(!complete.cases(x_init)) > 0)
      stop("x_init needs to be a complete dataframe with no missing values")
  } else { # check for non-custom
    if(!length(missForestObj$init) == length(vars_used))
      stop("Column names for new data should be the same as in the imputation initialization")
    if(!all(sort(names(missForestObj$init)) == sort(vars_used)))
      stop("Column names for new data should be the same as in the imputation initialization")
    if(!all(unlist(lapply(lapply(missForestObj$models, names),
                          function(x) sort(x) == sort(vars_included_to_impute)))))
      stop("Column names for new data should be the same as in the imputation initialization")
  }

  # initialize
  if (missForestObj$initialization != "custom") {
    ximp <- newdata

    if (length(missForestObj$integer_columns) > 0) {
      # make all integer columns double (imputed values might not be integer)
      ximp[missForestObj$integer_columns] <- lapply(ximp[, missForestObj$integer_columns, drop = FALSE],
                                                    as.double)
    }

    for (c in vars_used){

      # if new observations have different factor levels than on the learned data, add the levels to the factor
      if (is.factor(ximp[,c, drop = TRUE]) &
          !(missForestObj$init[[c]] %in% levels(ximp[,c, drop = TRUE])))
        levels(ximp[,c]) <- union(levels(ximp[,c, drop = TRUE]), missForestObj$init[[c]])

      ximp[is.na(ximp[,c, drop = TRUE]),c] <- missForestObj$init[[c]]
    }
  } else {
    ximp <- x_init

    if (length(missForestObj$integer_columns) > 0) {
      # make all integer columns double (imputed values might not be integer)
      ximp[missForestObj$integer_columns] <- lapply(ximp[, missForestObj$integer_columns, drop = FALSE],
                                                    as.double)
    }
  }

  # check that initialization is complete
  if (any(is.na(ximp[,vars_included_to_impute, drop = FALSE])))
    stop("Something went wrong in initialization")

  NAloc <- is.na(newdata)
  n_iter <- length(missForestObj$models)
  if (n_iter < missForestObj$maxiter) n_iter <- n_iter - 1 # take up to the model that converged (not the last one that was worse)
  impute_sequence <- missForestObj$impute_sequence
  impute_sequence <- impute_sequence[impute_sequence %in% vars_included_to_impute]

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
            if (is.factor(ximp[, c, drop = TRUE]))
              levels(ximp[, c]) <- unique(c(levels(ximp[,c, drop = TRUE]), levels))
            ximp[NAloc[,c],c] <- preds

          } else {

            ximp[NAloc[,c],c] <- predict(model, ximp[NAloc[,c], names(ximp)!=c])$predictions
          }
        }
      }
    }
  }

  if (missForestObj$return_integer_as_integer &
      length(missForestObj$integer_columns) > 0) {
    ximp[, missForestObj$integer_columns] <- lapply(ximp[, missForestObj$integer_columns, drop = FALSE],
                                                    function(x) as.integer(round(x)))
  }

  return(ximp)

}
