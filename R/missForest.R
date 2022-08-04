#' Imputes a dataframe and returns imputation models to be used on new observations
#'
#' Imputes a dataframe and returns imputation models to be used on new observations.
#' Models are built for each variable in the dataframe (even if there are no missing values).
#'
#' An adaptation of the original missForest algorithm (Stekhoven et al. 2012) is used.
#' Variables are initialized with a mean/mode, median/mode or custom imputation.
#' Then, they are imputed iteratively "on the fly" for a maximum number of iterations or until the convergence criteria are met.
#' The imputation sequence is either increasing or decreasing.
#' At each iteration, a random forest model is build for each variable using as outcome on the observed (non-missing) values
#' of the variable and as predictors the values of the other variables from previous iteration
#' for the first variable in the sequence or current iteration for next variables in the sequence
#' (on-the-fly). The ranger package (Wright et al. 2017) is used for building the random forest models.
#'
#' The convergence criterion is based on the out-of-boostrap (OOB) error and uses NMSE (normalized mean squared error)
#' for both continuous and categorical variables.
#'
#' Imputation models for all variables and all iterations are saved and can be later
#' applied to new observations.
#'
#' Both dataframe and tibble (tbl_df class) are supported as input. The imputed dataframe will be retured with the same class.
#' Numeric and integer columns are supported and treated internally as continuous variables.
#' Factor and character columns are supported and treated internally as categorical variables.
#' Other types (like boolean or dates) are not supported.
#' NA values are considered missing values.
#'
#' @param xmis dataframe containing missing values of class dataframe ("tibble" class tbl_df is also supported). Matrix format is not supported. See details for column format.
#' @param maxiter maximum number of iterations
#' @param OOB_weights named vector of weights for each variable in the convergence criteria.
#' The names should correspond to variable names. If predictor_matrix is used, it is sufficient to specify the
#' variables for which imputation needs to be done (have rowsum in the matrix > 0).
#' By default the weights are set to the proportion of missing values on each variable.
#' @param decreasing (boolean) if TRUE the order in which the variables are imputed is by decreasing amount of missing values.
#' (the variable with highest amount of missing values will be imputed first). If FALSE the variable with lowest amount of missing values will be imputed first.
#' @param force TODO: this is used by me for comparison tests; will be removed at the end.
#' @param initialization initialization method before running RF models; supported: mean/mode, median/mode and custom. Default is mean/mode.
#' @param x_init if \code{initialization = custom}; a complete dataframe to be used as initialization (see vignette for example).
#' @param class.weights a named list containing \code{class.weights} parameter to be passed to ranger for categorical variables.
#' The names of the list needs to respect the names of the categorical variables in the dataframe.
#' (See \code{ranger} function documentation in \code{ranger} package for details).
#' @param return_integer_as_integer Internally, integer columns are treated as double (double precision floating point numbers).
#' If TRUE, the imputations will be rounded to closest integer and returned as integer (This might be desirable for count variables).
#' If FALSE, integer columns will be returned as double (This might be desirable, for example, for patient age imputation).
#' Default is FALSE. The same behaviour will be applied to new observations when using missForestPredict.
#' @param save_models if TRUE, imputation models are saved and a new observation (or a test set) can be imputed using the models learned;
#' saving models on a dataset with a high number of variables will occupy RAM memory on the machine
#' @param predictor_matrix predictor matrix indicating which variables to use in the imputation of each variable.
#' See documentation for function `create_predictor_matrix` for details on the matrix format.
#' @param verbose (boolean) if TRUE then missForest returns OOB error estimates (MSE and NMSE) and runtime.
#' @param ... other arguments passed to ranger function (some arguments that are specific to each variable type are not supported).
#' See vignette for \code{num.trees} example.
#'
#' @return Object of class \code{missForest} with elements
#'     \item{\code{ximp}}{dataframe with imputed values}
#'     \item{\code{init}}{x_init if custom initalization is used; otherwise list of mean/mode or median/mode for each variable}
#'     \item{\code{initialization}}{value of initialization parameter}
#'     \item{\code{impute_sequence}}{vector variable names in the order in which imputation has been run}
#'     \item{\code{maxiter}}{maxiter parameter as passed to the function}
#'     \item{\code{models}}{list of random forest models for each iteration}
#'     \item{\code{return_integer_as_integer}}{Parameter return_integer_as_integer as passed to the function}
#'     \item{\code{integer_columns}}{list of columns of integer type in the data}
#'     \item{\code{err_MSE}}{dataframe with MSE (mean square error) values for each iteration and each variable}
#'     \item{\code{err_NMSE}}{dataframe with NMSE (normalized mean square error) values for each iteration and each variable}
#' @references
#' \itemize{
#'     \item Stekhoven, D. J., & Bühlmann, P. (2012). MissForest-non-parametric missing value imputation for mixed-type data. Bioinformatics, 28(1), 112-118. \doi{10.1093/bioinformatics/btr597}
#'     \item Wright, M. N. & Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C++ and R. J Stat Softw 77:1-17. \doi{10.18637/jss.v077.i01}.
#'   }
#' @examples
#' data(iris)
#' iris_mis <- produce_NA(iris, proportion = 0.1)
#' imputation_object <- missForest(iris_mis)
#' iris_imp <- imputation_object$ximp
#'
#' @import ranger
#' @importFrom stats median
#' @importFrom stats predict
#' @importFrom stats var
#' @importFrom stats complete.cases
#' @importFrom stats weighted.mean
#' @export

missForest <- function(xmis,
                       maxiter = 10,
                       OOB_weights = NULL,
                       decreasing = FALSE,
                       force = FALSE,
                       initialization = "mean/mode",
                       x_init = NULL,
                       class.weights = NULL,
                       return_integer_as_integer = FALSE,
                       save_models = FALSE,
                       predictor_matrix = NULL,
                       verbose = TRUE,
                       ...){

  unsupported_args <- c("case.weights", "splitrule", "num.random.splits",
                        "alpha", "minprop", "split.select.weights", "always.split.variables",
                        "inbag", "holdout", "quantreg", "oob.error", "dependent.variable.name",
                        "status.variable.name", "classification")

  unsupported_args <- unsupported_args[unsupported_args %in% names(list(...))]
  if (length(unsupported_args) > 0){
    stop(sprintf("The following argument(s) to ranger function are not supported: %s", paste(unsupported_args, collapse = ", ")))
  }

  if (!initialization %in% c("mean/mode", "median/mode", "custom"))
    stop("initialization has to be mean/mode, median/mode or custom")

  if (initialization == "custom"){
    if (is.null(x_init))
      stop("An initialized dataframe has to be provided in x_init if the initialization mode is custom")
    if (any(any(dim(xmis) != dim(x_init))))
      stop(sprintf("x_init needs to have the same dimensions as xmis: %s", paste(dim(xmis), collapse = ", ")))
    if (sum(!complete.cases(x_init)) > 0){
      stop("x_init needs to be a complete dataframe with no missing values")
    }
  }

  # prepare predictor matrix
  if (!is.null(predictor_matrix)){
    check_predictor_matrix(predictor_matrix, xmis, verbose = FALSE)
  } else {
    predictor_matrix <- create_predictor_matrix(xmis)
  }

  # variables included / skipped from imputation
  vars_skipped_to_impute <- rownames(predictor_matrix)[rowSums(predictor_matrix) == 0]
  vars_included_to_impute <- rownames(predictor_matrix)[!rowSums(predictor_matrix) == 0]
  # variables used / not used as predictors
  vars_skipped_as_pred <- colnames(predictor_matrix)[colSums(predictor_matrix) == 0]
  vars_included_as_pred <- colnames(predictor_matrix)[!colSums(predictor_matrix) == 0]
  vars_used <- unique(c(vars_included_to_impute, vars_included_as_pred))

  if (!is.null(OOB_weights)){

    if (is.null(names(OOB_weights)))
      stop("OOB_weights needs to be a named vector.")

    is_in_OOB_weights <- vars_included_to_impute %in% names(OOB_weights)

    if (!all(is_in_OOB_weights))
      stop("Variables %s need to be imputed and are not specified in OOB_weights.",
           vars_included_to_impute[!is_in_OOB_weights])

  }

  if (any(sapply(xmis[,vars_used], simplify = 'matrix', is.infinite)))
    stop("The dataframe contains Inf values. Inf values are not supported.")

  NA_only_cols <- colSums(is.na(xmis)) == nrow(xmis)

  if (any(NA_only_cols))
    stop(sprintf("There are columns that contain only missing values: %s",
                 paste(names(NA_only_cols)[NA_only_cols], collapse = ", ")))

  # check variable types
  p <- ncol(xmis)
  col_names <- colnames(xmis)
  #col_names <- col_names[col_names %in% vars_included_to_impute]

  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x) | is.character(x), "factor", NA_character_))

  var_type <- unlist(lapply(xmis[,vars_used], column_class))

  if (any(is.na(var_type)))
    stop(sprintf("Only numeric or factor columns are supported. Logical or other types are not supported. Unsupported variables: %s",
                 paste(names(var_type)[is.na(var_type)], collapse = ", ")))

  # check class.weights for factor variables
  if(!is.null(class.weights)){
    if (!is.list(class.weights) | is.null(names(class.weights)) )
      stop("class.weights needs to be a named list.")

    factor_vars <- names(var_type[var_type == "factor"])
    is_in_class_weights <- factor_vars %in% names(class.weights)

    if(!all(is_in_class_weights))
      stop("All categorical variables need to be in class.weights. Following variables not in class.weights.: %s",
           factor_vars[!is_in_class_weights])

    for (c in factor_vars) {
      n_classes <- length(unique(xmis[!is.na(xmis[,c, drop = TRUE]), c, drop = TRUE]))
      if (length(class.weights[[c]]) != n_classes)
        stop(sprintf("class.weights for variable %s needs to be a vector of size %s (the number of classes in the variable), not %s",
                     c, n_classes, length(class.weights[[c]])))
    }
  }

  # check factor / character with many levels
  for (c in vars_included_to_impute) {
    if (var_type[c] == "factor"){
      unique_levels <- length(unique(xmis[, c, drop = TRUE]))
      if (unique_levels > 53)
        message(sprintf("Variable %s has %s levels. Imputation might be slow. Check that variables are not factor or character by mistake.",
                        c, unique_levels))
    }
  }

  # perform initialization (mean/mode imputation)
  if (initialization == "custom") {
    ximp <- x_init
    if (save_models) var_init <- x_init

    # make all integer columns double (imputed values might not be integer)
    integer_columns <- colnames(ximp)[unlist(lapply(ximp, is.integer))]
    if (length(integer_columns) > 0) {
      ximp[, integer_columns] <- lapply(ximp[, integer_columns, drop = FALSE], as.double)
    }

  } else {
    ximp <- xmis

    # make all integer columns double (imputed values might not be integer)
    integer_columns <- colnames(ximp[,vars_used])[unlist(lapply(ximp[,vars_used], is.integer))]
    if (length(integer_columns) > 0) {
      ximp[, integer_columns] <- lapply(ximp[, integer_columns, drop = FALSE], as.double)
    }

    var_init <- vector("list", length(vars_used))
    names(var_init) <- vars_used

    for (col in vars_used) {
      if (var_type[[col]] == "numeric") {
        # keep mean or median
        if (initialization == "mean/mode"){
          mean_col <- mean(xmis[, col, drop = TRUE], na.rm = TRUE)
        } else if (initialization == "median/mode"){
          mean_col <- median(xmis[, col, drop = TRUE], na.rm = TRUE)
        }

        if (save_models) var_init[[col]] <- mean_col

        # initialize ximp column
        ximp[is.na(xmis[, col, drop = TRUE]), col] <- mean_col

      } else { # factor
        # take maximum number of samples in one class (ignore NA)
        max_level <- max(table(ximp[, col, drop = TRUE], useNA = "no"))
        summary_col <- summary(as.factor(ximp[!is.na(ximp[, col, drop = TRUE]), col, drop = TRUE]))
        # if there are several classes with equal number of samples, sample one at random
        mode_col <- sample(names(which(max_level == summary_col)), 1)
        # keep mode
        if (save_models) var_init[[col]] <- mode_col

        # initialize ximp column
        ximp[is.na(xmis[, col, drop = TRUE]),col] <- mode_col

      }
    }
  }

  # extract missingness pattern
  NAloc <- is.na(xmis)            # where are missings
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  miss_proportion <- noNAvar/nrow(xmis)
  # imputation sequence, first the lowest missingness column if decreasing = FALSE
  impute_sequence <- names(sort(noNAvar, decreasing = decreasing))
  impute_sequence <- impute_sequence[impute_sequence %in% vars_included_to_impute]

  # set weights
  OOB_weights <- miss_proportion
  if (sum(OOB_weights) == 0)
    OOB_weights <- replace(OOB_weights, names(OOB_weights), 1)
  OOB_weights <- OOB_weights[names(OOB_weights) %in% vars_included_to_impute]

  # keep MSE and NMSE
  err_MSE <- data.frame(matrix(ncol = length(vars_included_to_impute), nrow = 0))
  colnames(err_MSE) <- vars_included_to_impute
  err_NMSE <- data.frame(matrix(ncol = length(vars_included_to_impute), nrow = 0))
  colnames(err_NMSE) <- vars_included_to_impute

  iter <- 1
  convergence_criteria <- TRUE
  if (save_models) models <- list() else models <- NULL

  if (verbose) {
    if (length(vars_skipped_to_impute) > 0) {
      vars_skipped_prop <- sapply(vars_skipped_to_impute, function(x) sprintf("%s (%s)", x, miss_proportion[[x]]))
      cat("Variables skipped from imputation (missing proportion): ", vars_skipped_prop, "\n")
    }

    impute_sequence_prop <- sapply(impute_sequence, function(x) sprintf("%s (%s)", x, miss_proportion[[x]]))
    cat("Imputation sequence (missing proportion): ", impute_sequence_prop, "\n")
  }

  # iterate RF models
  while (convergence_criteria & iter <= maxiter){

    if (save_models) models[[iter]] <- list()

    if (verbose) cat("  missForest iteration", iter, "in progress...")

    t_start <- proc.time()
    # TODO: is it possible not to store this?
    ximp_old <- ximp

    for (col in impute_sequence){

      obsi <- !NAloc[, col]
      misi <- NAloc[, col]
      obsY <- ximp[obsi, col, drop = TRUE]

      predictor_cols <- colnames(predictor_matrix)[predictor_matrix[col,] == 1]
      obsX <- ximp[obsi, predictor_cols]
      misX <- ximp[misi, predictor_cols]

      if (var_type[col] == "numeric") {

        RF <- ranger(x = obsX, y = obsY, ...)

        # save model
        if (save_models) models[[iter]][[col]] <- RF

        # if the column is not complete, impute the missing values
        if (nrow(misX) > 0) {
          misY <- predict(RF, misX)$predictions
          ximp[misi, col] <- misY
        }

        # save the OOB error for convergence (NMSE)
        if (var(ximp[obsi, col]) != 0){ # contsant columns have 0 variance
          err_NMSE[iter, col] <- RF$prediction.error / var(ximp[obsi, col])
        } else {
          err_NMSE[iter, col] <- 0
        }
        # save the OOB error (MSE)
        oob_i <- !is.na(RF$predictions)
        if (sum(oob_i) != length(RF$predictions)){
          warning(sprintf("For variable %s there are %s observations out of %s for which the OOB error cannot be evaluated. These observations are excluded. Consider increasing the number of trees (num.trees) to ensure reliable results",
                          col, dim(RF$predictions)[[1]] - sum(oob_i), dim(RF$predictions)[[1]]))
        }

        err_MSE[iter, col] <- mse(RF$predictions[oob_i], obsY[oob_i])

      } else {
        obsY <- factor(obsY, levels = unique(ximp[, col, drop = TRUE]))

        RF <- ranger(x = obsX, y = obsY,
                     probability = TRUE,
                     class.weights = class.weights[[col]],
                     ...)

        # save model
        if (save_models) models[[iter]][[col]] <- RF

        # if the column is not complete, impute the missing values
        if (nrow(misX) > 0) {
          # if factor, return factor
          preds <- predict(RF, misX)$predictions
          levels <- colnames(preds)

          # impute
          ximp[misi, col] <- apply(preds, 1, function(x) levels[which.max(x)])
        }

        # save OOB error
        # drop levels if they don't exist
        ximp_binary <- make_binary(factor(ximp[, col, drop = TRUE], levels = unique(ximp[, col, drop = TRUE])))
        col_order <- colnames(ximp_binary)
        obsY_binary <- ximp_binary[obsi, , drop = FALSE]

        # predictions can be NaN when some observations are never out-of-bag
        oob_i <- !apply(is.na(RF$predictions), 1, any)
        if (sum(oob_i) != dim(RF$predictions)[[1]]){
          warning(sprintf("For variable %s there are %s observations out of %s for which the OOB error cannot be evaluated. These observations are excluded. Consider increasing the number of trees (num.trees) to ensure reliable results",
                          col, dim(RF$predictions)[[1]] - sum(oob_i), dim(RF$predictions)[[1]]))
        }

        # save OOB error (NMSE)
        err_NMSE[iter, col] <- BSnorm(RF$predictions[oob_i,col_order], obsY_binary[oob_i, , drop = FALSE])
        # save OOB error (MSE = Brier score divided by number of categories)
        err_MSE[iter, col] <- BS(RF$predictions[oob_i,col_order], obsY_binary[oob_i, , drop = FALSE]) /
          ncol(obsY_binary[oob_i, , drop = FALSE])
      }
    }

    if (verbose) cat("done!\n")

    # check convergence
    NMSE_err_new <- weighted.mean(err_NMSE[iter,], w = OOB_weights)
    if (iter == 1) {
      NMSE_err_old <- weighted.mean(rep(1, length(vars_included_to_impute)), w = OOB_weights)
    } else {
      NMSE_err_old <- weighted.mean(err_NMSE[iter - 1,], w = OOB_weights)
    }

    convergence_criteria <- NMSE_err_new < NMSE_err_old | force

    # return error monitoring
    if (verbose){
      delta_start <- proc.time() - t_start
      cat(sprintf("    OOB errors MSE:             %s\n",
                  paste(round(err_MSE[iter,], 10), collapse = ", ")))
      cat(sprintf("    OOB errors NMSE:            %s\n",
                  paste(round(err_NMSE[iter,], 10), collapse = ", ")))
      cat(sprintf("    (weigthed) difference NMSE: %s\n",
                  paste(round(NMSE_err_old - NMSE_err_new, 10), collapse = ", ")))
      cat(sprintf("    time:                       %s seconds\n\n",
                  round(delta_start[3], 10)))
    }

    iter <- iter + 1

  } # end while

  # return integer columns as integer
  if (return_integer_as_integer & length(integer_columns) > 0) {
    ximp[, integer_columns] <- lapply(ximp[, integer_columns, drop = FALSE],
                                      function(x) as.integer(round(x)))
    ximp_old[, integer_columns] <- lapply(ximp_old[, integer_columns, drop = FALSE],
                                          function(x) as.integer(round(x)))
  }

  # output
  if (iter != maxiter & !convergence_criteria) ximp <- ximp_old

  out <- list(ximp = ximp,
              init = var_init,
              initialization = initialization,
              impute_sequence = impute_sequence,
              maxiter = maxiter,
              save_models = save_models,
              models = models,
              return_integer_as_integer = return_integer_as_integer,
              integer_columns = integer_columns,
              err_MSE = err_MSE,
              err_NMSE = err_NMSE)

  class(out) <- 'missForest'
  return(out)
}
