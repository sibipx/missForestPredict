

#' Imputes a matrix / dataframe and returns imputation models to be used on new observations
#'
#' @param xmis matrix / dataframe containing missing values
#' @param maxiter maximum number of iterations
#' @param decreasing (boolean) if TRUE the columns are sorted with decreasing amount of missing values
#' @param initialization initialization method before running RF models; supported: mean/mode, median/mode and custom
#' @param x_init if initialization = custom; a compplete dataframe to be used as initialization
#' @param verbose (boolean) if TRUE then missForest returns error estimates, runtime and if available true error during iterations
#' @param ... other arguments passed to ranger function (some arguments that are specific to each variable type are not supported)
#'
#' @return TODO
#' @examples
#' data(iris)
#' iris_mis <- prodNA(iris, noNA = 0.1)
#' imputation_object <- missForest(iris_mis)
#' iris_imp <- imputation_object$ximp
#' @export

missForest <- function(xmis,
                       maxiter = 10,
                       decreasing = FALSE,
                       force = FALSE,
                       initialization = "median/mode",
                       x_init = NULL,
                       verbose = FALSE,
                       ...){

  unsupported_args <- c("case.weights", "class.weights", "splitrule", "num.random.splits",
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


  p <- ncol(xmis)
  col_names <- colnames(xmis)

  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == nrow(xmis)))
    stop("There are variables completely missing in the input data. Remove these before imputation")

  # check variable types
  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x) | is.character(x), "factor", NA_character_))

  varType <- unlist(lapply(xmis, column_class))

  if (any(is.na(varType))) stop("Only numeric or factor columns are supported. Logical or other types are not supported.")

  # perform initialization (mean/mode imputation)
  if (initialization == "custom") {
    ximp <- x_init
    var_single_init <- NULL

    # make all integer columns double (imputed values might not be integer)
    ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)

  } else {
    ximp <- xmis

    # make all integer columns double (imputed values might not be integer)
    ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)

    var_single_init <- vector("list", p)
    names(var_single_init) <- col_names

    for (col in col_names) {
      if (varType[[col]] == "numeric") {
        # keep mean or median
        if (initialization == "mean/mode"){
          mean_col <- mean(xmis[, col, drop = TRUE], na.rm = TRUE)
        } else if (initialization == "median/mode"){
          mean_col <- median(xmis[, col, drop = TRUE], na.rm = TRUE)
        }

        var_single_init[[col]] <- mean_col

        # initialize ximp column
        ximp[is.na(xmis[, col, drop = TRUE]), col] <- mean_col

      } else { # factor
        # take maximum number of samples in one class (ignore NA)
        max_level <- max(table(ximp[, col, drop = TRUE], useNA = "no"))
        summary_col <- summary(as.factor(ximp[!is.na(ximp[, col, drop = TRUE]), col, drop = TRUE]))
        # if there are several classes with equal number of samples, sample one at random
        mode_col <- sample(names(which(max_level == summary_col)), 1)
        # keep mode
        var_single_init[[col]] <- mode_col

        # initialize ximp column
        ximp[is.na(xmis[, col, drop = TRUE]),col] <- mode_col

      }
    }
  }

  # extract missingness pattern
  NAloc <- is.na(xmis)            # where are missings
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  # imputation sequence, first the lowest missingness column if decreasing = FALSE
  impute_sequence <- names(sort(noNAvar, decreasing = decreasing))

  # initialize vectors to trace the errors for each column
  iter <- 0
  err_new <- rep(0, p)
  names(err_new) <- col_names
  err_old <- rep(1, p) # was 1 for OOB 1 is as good as chance // rep(Inf, p)
  OOBerrOld <-  1 # was 1 for OOB 1 is as good as chance
  err_OOB_corrected_old <- 1
  names(err_old) <- col_names
  err_OOB <- numeric(p)
  names(err_OOB) <- col_names

  err_OOB_corrected <- numeric(p)
  names(err_OOB_corrected) <- col_names

  models <- list()

  ## iterate missForest
  while ((sum(err_new) < sum(err_old) | force) & iter < maxiter){

    models[[iter + 1]] <- list()

    if (iter != 0){
      err_old <- err_new
      OOBerrOld <- err_OOB
      err_OOB_corrected_old <- err_OOB_corrected
    }

    if (verbose) cat("  missForest iteration", iter+1, "in progress...")

    t.start <- proc.time()
    ximp.old <- ximp

    for (col in impute_sequence){

      obsi <- !NAloc[, col]
      misi <- NAloc[, col]
      obsY <- ximp[obsi, col, drop = TRUE]
      obsX <- ximp[obsi, names(ximp)!=col]
      misX <- ximp[misi, names(ximp)!=col]

      if (varType[col] == "numeric") {

        RF <- ranger(x = obsX, y = obsY, ...)

        # record out-of-bag error (MSE)
        err_OOB[col] <- RF$prediction.error

        # save model
        models[[iter + 1]][[col]] <- RF

        # if the column is not complete, impute the missing values
        if (nrow(misX) > 0) {
          misY <- predict(RF, misX)$predictions
        } else {
          misY <- c()
        }
        ximp[misi, col] <- misY

        # save the OOB error for convergence (NMSE)
        err_new[col] <- err_OOB[col] / var(ximp[obsi, col])
        #err_new[col] <- nmse(RF$predictions, ximp[obsi, col]) # this should be the same
        # save the OOB error (MSE)
        err_OOB_corrected[col] <- mse(RF$predictions, ximp[obsi, col, drop = TRUE])

      } else {
        obsY <- factor(obsY, levels = unique(ximp[, col, drop = TRUE]))
        summarY <- summary(obsY)
        if (length(summarY) == 1) {
          misY <- factor(rep(names(summarY), sum(misi)))
        } else {

          RF <- ranger(x = obsX, y = obsY, probability = TRUE, ...)

          # record out-of-bag error (Brier score when probability = TRUE)
          err_OOB[col] <- RF$prediction.error

          # save model
          models[[iter + 1]][[col]] <- RF

          # if the column is not complete, impute the missing values
          if (nrow(misX) > 0) {

            # if factor, return factor
            preds <- predict(RF, misX)$predictions
            levels <- colnames(preds)
            misY <- apply(preds, 1, function(x) levels[which.max(x)])

          } else {
            misY <- c()
          }
          ximp[misi, col] <- misY

          # save OOB error
          #ximp_binary <- make_binary(ximp[, col, drop = TRUE])
          # drop levels if they don't exist
          ximp_binary <- make_binary(factor(ximp[, col, drop = TRUE], levels = unique(ximp[, col, drop = TRUE])))
          col_order <- colnames(ximp_binary)

          # if a class is missing, add the class
          OOB_predictions <- RF$predictions

          #all_levels <- levels(ximp[, col, drop = TRUE])
          #miss_levels <- all_levels[!all_levels %in% colnames(OOB_predictions)]
          #if (length(miss_levels) > 0) {
          #  add_columns <- matrix(rep(0, length(miss_levels) * nrow(OOB_predictions)), nrow = nrow(OOB_predictions))
          #  colnames(add_columns) <- miss_levels
          #  OOB_predictions <- cbind(OOB_predictions, add_columns)
          #}

          # save OOB error (NMSE)
          # BS(RF$predictions, ximp_new_binary[obsi,]) # multiclass.Brier - THIS!
          # multiclass.Brier(RF$predictions, ximp[obsi,col]) # multiclass.Brier
          # BSnorm(RF$predictions, ximp_new_binary[obsi,]) # THIS!
          err_new[col] <- BSnorm(OOB_predictions[,col_order], ximp_binary[obsi,]) # preserve column order

          # save OOB error (MSE = Brier score divided by number of categories)
          err_OOB_corrected[col] <- BS(OOB_predictions[,col_order], ximp_binary[obsi,])/ncol(ximp_binary[obsi,])
        }
      }

    }

    if (verbose) cat('done!\n')

    iter <- iter + 1

    ## return status output, if desired
    if (verbose){
      delta.start <- proc.time() - t.start
      #cat("    OOB error(s) MSE (ranger):    ", err_OOB, "\n")
      # this is the OOB error(s) MSE (corrected)
      cat(sprintf("    OOB errors MSE:   %s\n", paste(err_OOB_corrected, collapse = ", ")))
      cat(sprintf("    OOB errors NMSE:  %s\n", paste(err_new, collapse = ", ")))
      cat(sprintf("    differences:      %s\n", paste(err_old - err_new, collapse = ", ")))
      cat(sprintf("    difference total: %s\n", paste(sum(err_old) - sum(err_new), collapse = ", ")))
      cat(sprintf("    time:             %s seconds\n\n", delta.start[3]))
    }
  }#end while

  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    out <- list(ximp = ximp, OOBerror = err_OOB, err_OOB_corrected = err_OOB_corrected)
  } else {
    out <- list(ximp = ximp.old, OOBerror = OOBerrOld, err_OOB_corrected = err_OOB_corrected_old)
  }

  # save single initialization as list
  out$init <- var_single_init
  out$initialization <- initialization
  out$models <- models
  out$impute_sequence <- impute_sequence
  out$maxiter <- maxiter

  class(out) <- 'missForest'
  return(out)
}
