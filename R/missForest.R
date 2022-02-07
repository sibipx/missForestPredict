#' Imputes a dataframe and returns imputation models to be used on new observations
#'
#' Imputes a dataframe and returns imputation models to be used on new observations.
#' Models are built for each variable in the dataframe (even if there are no missing values).
#'
#' @param xmis dataframe containing missing values
#' @param maxiter maximum number of iterations
#' @param OOB_weights vector of weights for each variable in the convergence criteria. By default all variables weight equally.
#' Considering that imputation models are built for all variables, even if no missing values are encountered, it can considered to adjust the
#' weights to 0 for those variables that are expected to never be missing. Alternatively the weights can be adjusted
#' according to the (expected) proportion of missingness on each variable.
#' @param decreasing (boolean) if TRUE the columns are sorted with decreasing amount of missing values
#' @param initialization initialization method before running RF models; supported: mean/mode, median/mode and custom
#' @param x_init if initialization = custom; a complete dataframe to be used as initialization
#' @param verbose (boolean) if TRUE then missForest returns error estimates and runtime
#' @param ... other arguments passed to ranger function (some arguments that are specific to each variable type are not supported)
#'
#' @return Object of class \code{missForest} with elements
#'     \item{\code{ximp}}{dataframe with imputed values}
#'     \item{\code{init}}{x_init if custom initalization is used; otherwise list of mean/mode or median/mode for each variable}
#'     \item{\code{initialization}}{value of initialization parameter}
#'     \item{\code{impute_sequence}}{vector variable names in the order in which imputation has been run}
#'     \item{\code{maxiter}}{maxiter parameter as passed to the function}
#'     \item{\code{models}}{list of random forest models for each iteration}
#'     \item{\code{err_MSE}}{dataframe with MSE values for each iteration and each variable}
#'     \item{\code{err_NMSE}}{dataframe with NMSE values for each iteration and each variable}
#'
#' @examples
#' data(iris)
#' iris_mis <- produce_NA(iris, proportion = 0.1)
#' imputation_object <- missForest(iris_mis)
#' iris_imp <- imputation_object$ximp
#' @export

missForest <- function(xmis,
                       maxiter = 10,
                       OOB_weights = NULL,
                       decreasing = FALSE,
                       force = FALSE,
                       initialization = "median/mode",
                       x_init = NULL,
                       verbose = TRUE,
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

  if (!is.null(OOB_weights) & length(OOB_weights) != ncol(xmis))
    stop(sprintf("OOB_weights has to be a vector of length %s (number of columns", ncol(xmis)))

  if (any(apply(is.na(xmis), 2, sum) == nrow(xmis)))
    stop("There are variables completely missing in the input data. Remove these before imputation")

  p <- ncol(xmis)
  col_names <- colnames(xmis)
  if(is.null(OOB_weights)) OOB_weights <- rep(1, p)

  # check variable types
  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x) | is.character(x), "factor", NA_character_))

  varType <- unlist(lapply(xmis, column_class))

  if (any(is.na(varType))) stop("Only numeric or factor columns are supported. Logical or other types are not supported.")

  # perform initialization (mean/mode imputation)
  if (initialization == "custom") {
    ximp <- x_init
    var_init <- x_init

    # make all integer columns double (imputed values might not be integer)
    ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)

  } else {
    ximp <- xmis

    # make all integer columns double (imputed values might not be integer)
    ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)

    var_init <- vector("list", p)
    names(var_init) <- col_names

    for (col in col_names) {
      if (varType[[col]] == "numeric") {
        # keep mean or median
        if (initialization == "mean/mode"){
          mean_col <- mean(xmis[, col, drop = TRUE], na.rm = TRUE)
        } else if (initialization == "median/mode"){
          mean_col <- median(xmis[, col, drop = TRUE], na.rm = TRUE)
        }

        var_init[[col]] <- mean_col

        # initialize ximp column
        ximp[is.na(xmis[, col, drop = TRUE]), col] <- mean_col

      } else { # factor
        # take maximum number of samples in one class (ignore NA)
        max_level <- max(table(ximp[, col, drop = TRUE], useNA = "no"))
        summary_col <- summary(as.factor(ximp[!is.na(ximp[, col, drop = TRUE]), col, drop = TRUE]))
        # if there are several classes with equal number of samples, sample one at random
        mode_col <- sample(names(which(max_level == summary_col)), 1)
        # keep mode
        var_init[[col]] <- mode_col

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

  # keep MSE and NMSE
  err_MSE <- data.frame(matrix(ncol = p, nrow = 0))
  colnames(err_MSE) <- col_names
  err_NMSE <- data.frame(matrix(ncol = p, nrow = 0))
  colnames(err_NMSE) <- col_names

  iter <- 1
  models <- list()
  convergence_criteria <- TRUE

  # iterate RF models
  while (convergence_criteria & iter <= maxiter){

    models[[iter]] <- list()

    if (verbose) cat("  missForest iteration", iter, "in progress...")

    t.start <- proc.time()
    ximp_old <- ximp

    for (col in impute_sequence){

      obsi <- !NAloc[, col]
      misi <- NAloc[, col]
      obsY <- ximp[obsi, col, drop = TRUE]
      obsX <- ximp[obsi, names(ximp)!=col]
      misX <- ximp[misi, names(ximp)!=col]

      if (varType[col] == "numeric") {

        RF <- ranger(x = obsX, y = obsY, ...)

        # save model
        models[[iter]][[col]] <- RF

        # if the column is not complete, impute the missing values
        if (nrow(misX) > 0) {
          misY <- predict(RF, misX)$predictions
        } else {
          misY <- c()
        }
        ximp[misi, col] <- misY

        # save the OOB error for convergence (NMSE)
        err_NMSE[iter, col] <- RF$prediction.error / var(ximp[obsi, col])
        # save the OOB error (MSE)
        err_MSE[iter, col] <- mse(RF$predictions, ximp[obsi, col, drop = TRUE])

      } else {
        obsY <- factor(obsY, levels = unique(ximp[, col, drop = TRUE]))
        summarY <- summary(obsY)
        if (length(summarY) == 1) {
          misY <- factor(rep(names(summarY), sum(misi)))
        } else {

          RF <- ranger(x = obsX, y = obsY, probability = TRUE, ...)

          # save model
          models[[iter]][[col]] <- RF

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
          # drop levels if they don't exist
          ximp_binary <- make_binary(factor(ximp[, col, drop = TRUE], levels = unique(ximp[, col, drop = TRUE])))
          col_order <- colnames(ximp_binary)

          # save OOB error (NMSE)
          err_NMSE[iter, col] <- BSnorm(RF$predictions[,col_order], ximp_binary[obsi,])
          # save OOB error (MSE = Brier score divided by number of categories)
          err_MSE[iter, col] <- BS(RF$predictions[,col_order], ximp_binary[obsi,])/ncol(ximp_binary[obsi,])
        }
      }
    }

    if (verbose) cat('done!\n')

    # check convergence
    NMSE_err_new <- weighted.mean(err_NMSE[iter,], w = OOB_weights)
    if (iter == 1) {
      NMSE_err_old <- weighted.mean(rep(1, p), w = OOB_weights)
    } else {
      NMSE_err_old <- weighted.mean(err_NMSE[iter - 1,], w = OOB_weights)
    }

    convergence_criteria <- NMSE_err_new < NMSE_err_old | force

    # return error monitoring
    if (verbose){
      delta.start <- proc.time() - t.start
      cat(sprintf("    OOB errors MSE:             %s\n", paste(err_MSE[iter,], collapse = ", ")))
      cat(sprintf("    OOB errors NMSE:            %s\n", paste(err_NMSE[iter,], collapse = ", ")))
      cat(sprintf("    (weigthed) difference NMSE: %s\n", paste(NMSE_err_old - NMSE_err_new, collapse = ", ")))
      cat(sprintf("    time:                       %s seconds\n\n", delta.start[3]))
    }

    iter <- iter + 1

  } # end while

  # output
  if (iter != maxiter) ximp <- ximp_old

  out <- list(ximp = ximp,
              init = var_init,
              initialization = initialization,
              impute_sequence = impute_sequence,
              maxiter = maxiter,
              models = models,
              err_MSE = err_MSE,
              err_NMSE = err_NMSE)

  class(out) <- 'missForest'
  return(out)
}
