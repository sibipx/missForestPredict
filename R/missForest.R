

#' Imputes a matrix / dataframe and returns imputation models to be used on new observations
#'
#' @param xmis matrix / dataframe containing missing values
#' @param maxiter maximum number of iterations
#' @param variablewise TODO: get rid of this
#' @param decreasing (boolean) if TRUE the columns are sorted with decreasing amount of missing values
#' @param verbose (boolean) if TRUE then missForest returns error estimates, runtime and if available true error during iterations
#' @param class.weights list of priors of the classes in the categorical variables
#' @param xtrue complete data matrix
#' @param ... other arguments passed to ranger function
#'
#' @return
#' @export

missForest <- function(xmis,
                       maxiter = 10,
                       variablewise = FALSE,
                       decreasing = FALSE,
                       verbose = FALSE,
                       class.weights = NULL,
                       xtrue = NA,
                       ...){

  p <- ncol(xmis)
  col_names <- colnames(xmis)

  # TODO: review class.weights
  if (!is.null(class.weights))
    stopifnot(length(class.weights) == p, typeof(class.weights) == 'list')

  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == nrow(xmis)))
    stop("There are variables completely missing in the input data. Remove these before imputation")

  # TODO: should I support character or not? For now only factor

  # check variable types
  column_class <- function(x) ifelse(is.numeric(x), "numeric",
                                     ifelse(is.factor(x), "factor", NA_character_))

  varType <- unlist(lapply(xmis, column_class))

  if (any(is.na(varType))) stop("Only numeric or factor columns are supported. Logical or other types are not supported.")

  # perform initialization (mean/mode imputation)
  ximp <- xmis

  # make all integer columns double (imputed values might not be integer)
  ximp[unlist(lapply(ximp, is.integer))] <- sapply(ximp[unlist(lapply(ximp, is.integer))],as.double)

  # TODO: give warning? test on diamonds

  var_single_init <- vector("list", p)
  names(var_single_init) <- col_names

  for (col in col_names) {
    if (varType[[col]] == "numeric") {
      # keep mean
      mean_col <- mean(xmis[, col, drop = TRUE], na.rm = TRUE)
      var_single_init[[col]] <- mean_col

      # initialize ximp column
      ximp[is.na(xmis[, col, drop = TRUE]), col] <- mean_col
    } else { # factor
      # take maximum number of samples in one class (ignore NA)
      max_level <- max(table(ximp[, col, drop = TRUE], useNA = "no"))
      summary_col <- summary(ximp[!is.na(ximp[, col, drop = TRUE]), col, drop = TRUE])
      # if there are several classes with equal number of samples, sample one at random
      mode_col <- sample(names(which(max_level == summary_col)), 1)
      # keep mode
      var_single_init[[col]] <- mode_col

      # initialize ximp column
      ximp[is.na(xmis[, col, drop = TRUE]),col] <- mode_col

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
  #err_old <- rep(Inf, p)
  err_old <- rep(Inf, p) # was 1 for OOB 1 is as good as chance
  #OOBerrOld <-  Inf # was 1 for OOB 1 is as good as chance
  names(err_old) <- col_names
  err_OOB <- numeric(p)
  names(err_OOB) <- col_names

  err_OOB_corrected <- numeric(p)
  names(err_OOB_corrected) <- col_names

  # gets a factor variable, returns a matrix of binary variables
  make_binary <- function(x) {

    unique_vals <- levels(x)
    x_binary <- matrix(ncol = length(unique_vals), nrow = length(x))

    for (i in 1:length(unique_vals)) {
      x_binary[,i] <- ifelse(x == unique_vals[[i]], 1, 0)
    }

    return(x_binary)

  }

  models <- list()

  ## iterate missForest
  while (sum(err_new) < sum(err_old) & iter < maxiter){

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
        # NMRSE
        #err_OOB[col] <- sqrt(RF$prediction.error/var(as.vector(as.matrix(xmis[, col])),
        #                                                 na.rm = TRUE))

        # TODO: I don't need matrix here for 1 var - I can just use drop = TRUE

        #sqrt(mean((ximp[mis] - xtrue[mis])^{2}) / var(xtrue[mis]))

        # save model
        models[[iter + 1]][[col]] <- RF

        if (nrow(misX) > 0) { # if the column is not complete
          misY <- predict(RF, misX)$predictions
        } else {
          misY <- c()
        }
        ximp[misi, col] <- misY
        # TODO: can I already update the column here? why not?

      } else {
        obsY <- factor(obsY)
        summarY <- summary(obsY)
        if (length(summarY) == 1) {
          misY <- factor(rep(names(summarY), sum(misi)))
        } else {

          RF <- ranger(x = obsX, y = obsY,
                       #class.weights = if (!is.null(class.weights)) class.weights[[varInd]], # TODO: test this and cutoff and strata
                       probability = TRUE,
                       ...)

          # record out-of-bag error (Brier score when probability = TRUE)
          err_OOB[col] <- RF$prediction.error


          # save model
          models[[iter + 1]][[col]] <- RF

          # predict missing parts of Y
          if (nrow(misX) > 0) { # if the column is not complete
            #misY <- predict(RF, misX)$predictions

            # if factor, return factor
            preds <- predict(RF, misX)$predictions
            levels <- colnames(preds)
            misY <- apply(preds, 1, function(x) levels[which.max(x)])

          } else {
            misY <- c()
          }
          ximp[misi, col] <- misY
        }
      }
      #ximp[misi, col] <- misY

      # save convergence
      if (varType[col] == "numeric"){

        err_new[col] <- err_OOB[col] / var(ximp[obsi, col])
        #err_new[col] <- nmse(RF$predictions, ximp[obsi, col]) # this should be the same
        err_OOB_corrected[col] <- mse(RF$predictions, ximp[obsi, col, drop = TRUE])

      } else {


        #ximp_old_binary <- make_binary(ximp.old[, col, drop = TRUE]) # ximp.old[, col, , drop = TRUE] // ximp.old[[col]]
        #ximp_new_binary <- make_binary(ximp[, col, drop = TRUE])

        ximp_binary <- make_binary(ximp[, col, drop = TRUE])

        # BS(RF$predictions, ximp_new_binary[obsi,]) # multiclass.Brier - THIS!
        # multiclass.Brier(RF$predictions, ximp[obsi,col]) # multiclass.Brier
        # BSnorm(RF$predictions, ximp_new_binary[obsi,]) # THIS!
        err_new[col] <- BSnorm(RF$predictions, ximp_binary[obsi,])

        err_OOB_corrected[col] <- BS(RF$predictions, ximp_binary[obsi,])/ncol(ximp_binary[obsi,])

      }

    }

    if (verbose) cat('done!\n')

    iter <- iter + 1

    ## compute estimated imputation error
    if (any(!is.na(xtrue))){
      err <- suppressWarnings(mixError(ximp, xmis, xtrue))
    }

    ## return status output, if desired
    if (verbose){
      delta.start <- proc.time() - t.start
      if (any(!is.na(xtrue))){
        cat("    error(s):", err, "\n")
      }
      cat("    OOB error(s) MSE (ranger):    ", err_OOB, "\n")
      cat("    OOB error(s) MSE (corrected): ", err_OOB_corrected, "\n")
      cat("    OOB error(s) NMSE:            ", err_new, "\n")
      cat("    difference(s):                ", err_old - err_new, "\n")
      cat("    difference(s) total:          ", sum(err_old) - sum(err_new) , "\n")

      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }#end while((err_new<err_old)&(iter<maxiter)){

  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    if (any(is.na(xtrue))){
      out <- list(ximp = ximp, OOBerror = err_OOB, err_OOB_corrected = err_OOB_corrected)
    } else {
      out <- list(ximp = ximp, OOBerror = err_OOB, error = err, err_OOB_corrected = err_OOB_corrected)
    }
  } else {
    if (any(is.na(xtrue))){
      out <- list(ximp = ximp.old, OOBerror = OOBerrOld, err_OOB_corrected = err_OOB_corrected_old)
    } else {
      out <- list(ximp = ximp.old, OOBerror = OOBerrOld, err_OOB_corrected = err_OOB_corrected_old,
                  error = suppressWarnings(mixError(ximp.old, xmis, xtrue)))
    }
  }

  # save single initialization as list
  out$init <- var_single_init
  out$models <- models
  out$impute_sequence <- impute_sequence
  out$iter <- iter
  out$maxiter <- maxiter
  #out$err_OOB_corrected <- err_OOB_corrected

  class(out) <- 'missForest'
  return(out)
}
