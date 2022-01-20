

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
  err_old <- rep(1, p)
  OOBerrOld <-  1
  names(err_old) <- col_names
  err_OOB <- numeric(p)
  names(err_OOB) <- col_names

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
    }

    cat("  missForest iteration", iter+1, "in progress...")

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
        #err_new[col] <- sum((ximp[, col] - ximp.old[, col])^2) / sum(ximp[, col]^2)
        # TODO: alternative: only check for misi?

        # normalized on old
        #err_new[col] <- sum((ximp[, col] - ximp.old[, col])^2) / sum((ximp.old[, col] - mean(ximp.old[, col]))^2)
        # normalized on new
        #err_new[col] <- sum((ximp[, col] - ximp.old[, col])^2) / sum((ximp[, col] - mean(ximp[, col]))^2)
        # only missing
        #err_new[col] <- sum((ximp[misi, col] - ximp.old[misi, col])^2) / sum((ximp[misi, col] - mean(ximp[, col]))^2)
        # OOB all
        #err_new[col] <- err_OOB[col] / var(ximp[, col])
        # OOB obs
        err_new[col] <- err_OOB[col] / var(ximp[obsi, col])

      } else {

        ximp_old_binary <- make_binary(ximp.old[, col, drop = TRUE]) # ximp.old[, col, , drop = TRUE] // ximp.old[[col]]
        ximp_new_binary <- make_binary(ximp[, col, drop = TRUE])

        ## normalized on old
        #mean_observed <- mean(ximp_old_binary) # old
        #bs_baseline <- mean((mean_observed - ximp_old_binary)^2)
        #err_new[col] <- mean((ximp_new_binary - ximp_old_binary)^2) / bs_baseline
        ## this should be 1 - BSS
        #
        ## normalized on new
        #mean_observed <- mean(ximp_new_binary) # new
        #bs_baseline <- mean((mean_observed - ximp_new_binary)^2)
        #err_new[col] <- mean((ximp_new_binary - ximp_old_binary)^2) / bs_baseline

        # only missing
        #mean_observed <- mean(ximp_new_binary) # new
        #bs_baseline <- mean((mean_observed - ximp_new_binary[misi,])^2)
        #err_new[col] <- mean((ximp_new_binary[misi,] - ximp_old_binary[misi,])^2) / bs_baseline

        # OOB
        #mean_observed <- mean(ximp_new_binary[obsi,]) # new
        #bs_baseline <- mean((mean_observed - ximp_new_binary[obsi,])^2)
        #err_new[col] <- err_OOB[col] / bs_baseline

        #inc = mean(ximp_new_binary[obsi,1])
        #brier.max = inc * (1 - inc)^2 + (1 - inc) * inc^2
        #1 - err_OOB[col]/brier.max

        bs_refs <- c()
        scores <- c()
        for (i in 1:dim(ximp_new_binary)[[2]]){
          m <- mean(ximp_new_binary[obsi,i])
          bs_ref <- sum((ximp_new_binary[obsi,i] - m)^2)/length(ximp_new_binary[obsi,i])
          bs_refs <- c(bs_refs, bs_ref)

          # brier binary
          #y = as.numeric(truth == positive)
          #mean((y - probabilities)^2)

          y <- ximp_new_binary[obsi,i]
          brier_binary <- mean((y - RF$predictions[,i])^2)
          nmse <- brier_binary/bs_ref
          scores <- c(scores, nmse)

        }
        #bs_baseline <- mean(bs_refs)

        #err_new[col] <- err_OOB[col] / bs_baseline

        err_new[col] <- mean(scores)

      }

    }

    cat('done!\n')

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
      cat("    OOB error(s) MSE:   ", err_OOB, "\n")
      cat("    OOB error(s) NMSE:  ", err_new, "\n")
      cat("    difference(s):      ", err_old - err_new, "\n")
      cat("    difference(s) total:", sum(err_old) - sum(err_new) , "\n")

      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }#end while((err_new<err_old)&(iter<maxiter)){

  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    if (any(is.na(xtrue))){
      out <- list(ximp = ximp, OOBerror = err_OOB)
    } else {
      out <- list(ximp = ximp, OOBerror = err_OOB, error = err)
    }
  } else {
    if (any(is.na(xtrue))){
      out <- list(ximp = ximp.old, OOBerror = OOBerrOld)
    } else {
      out <- list(ximp = ximp.old, OOBerror = OOBerrOld,
                  error = suppressWarnings(mixError(ximp.old, xmis, xtrue)))
    }
  }

  # save single initialization as list
  out$init <- var_single_init
  out$models <- models
  out$impute_sequence <- impute_sequence
  out$iter <- iter
  out$maxiter <- maxiter

  class(out) <- 'missForest'
  return(out)
}
