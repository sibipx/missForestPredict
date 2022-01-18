

#' Imputes a matrix / dataframe and returns imputation models to be used on new observations
#'
#' @param xmis matrix / dataframe containing missing values
#' @param maxiter maximum number of iterations
#' @param ntree number of trees in the forest (default 500)
#' @param variablewise logical; return OOB errors for each variable separately
#' @param decreasing (boolean) if TRUE the columns are sorted with decreasing amount of missing values
#' @param verbose (boolean) if TRUE then missForest returns error estimates, runtime and if available true error during iterations
#' @param mtry how many variables should be tried randomly at each node
#' @param class.weights list of priors of the classes in the categorical variables
#' @param xtrue complete data matrix
#'
#' @return
#' @export

missForest <- function(xmis,
                       maxiter = 10,
                       ntree = 500,
                       variablewise = FALSE,
                       decreasing = FALSE,
                       verbose = FALSE,
                       mtry = floor(sqrt(ncol(xmis))),
                       class.weights = NULL,
                       xtrue = NA) {

  n <- nrow(xmis)
  p <- ncol(xmis)
  col_names <- colnames(xmis)

  # TODO: review class.weights
  if (!is.null(class.weights))
    stopifnot(length(class.weights) == p, typeof(class.weights) == 'list')

  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == n))
    stop("There are variables completely missing in the input data. Remove these before imputation")

  # TODO: if matrix, make dataframe. OR test with matrix
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
      mean_col <- mean(xmis[,col], na.rm = TRUE)
      var_single_init[[col]] <- mean_col

      # initialize ximp column
      ximp[is.na(xmis[,col]),col] <- mean_col
    } else { # factor
      # take maximum number of samples in one class (ignore NA)
      max_level <- max(table(ximp[[col]], useNA = "no"))
      summary_col <- summary(ximp[[col]][!is.na(ximp[[col]])])
      # if there are several classes with equal number of samples, sample one at random
      mode_col <- sample(names(which(max_level == summary_col)), 1)
      # keep mode
      var_single_init[[col]] <- mode_col

      # initialize ximp column
      ximp[is.na(xmis[[col]]),col] <- mode_col

    }
  }

  # extract missingness pattern
  NAloc <- is.na(xmis)            # where are missings
  noNAvar <- apply(NAloc, 2, sum) # how many are missing in the vars
  sort.j <- order(noNAvar)        # indices of increasing amount of NA in vars
  if (decreasing)
    sort.j <- rev(sort.j)
  sort.noNAvar <- noNAvar[sort.j]

  ## output
  Ximp <- vector('list', maxiter)

  ## initialize parameters of interest
  iter <- 0
  k <- length(unique(varType))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  OOBerror <- numeric(p)
  names(OOBerror) <- varType

  ## setup convergence variables w.r.t. variable types
  if (k == 1){
    if (unique(varType) == 'numeric'){
      names(convNew) <- c('numeric')
    } else {
      names(convNew) <- c('factor')
    }
    convergence <- c()
    OOBerr <- numeric(1)
  } else {
    names(convNew) <- c('numeric', 'factor')
    convergence <- matrix(NA, ncol = 2)
    OOBerr <- numeric(2)
  }

  ## function to yield the stopping criterion in the following 'while' loop
  stopCriterion <- function(varType, convNew, convOld, iter, maxiter){
    k <- length(unique(varType))
    if (k == 1){
      (convNew < convOld) & (iter < maxiter)
    } else {
      #((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
      (convNew[1] + convNew[2] < convOld[1] + convOld[2])  & (iter < maxiter)
    }
  }

  models <- list()
  ## iterate missForest
  while (stopCriterion(varType, convNew, convOld, iter, maxiter)){

    models[[iter + 1]] <- list()

    if (iter != 0){
      convOld <- convNew
      OOBerrOld <- OOBerr
    }

    cat("  missForest iteration", iter+1, "in progress...")

    t.start <- proc.time()
    ximp.old <- ximp

    for (s in 1 : p) {
      varInd <- sort.j[s]

      obsi <- !NAloc[, varInd]
      misi <- NAloc[, varInd]
      obsY <- ximp[obsi, varInd]
      obsX <- ximp[obsi, seq(1, p)[-varInd]]
      misX <- ximp[misi, seq(1, p)[-varInd]]
      typeY <- varType[varInd]
      if (typeY == "numeric") {

        #RF <- randomForest( x = obsX,
        #                    y = obsY,
        #                    ntree = ntree,
        #                    mtry = mtry,
        #                    replace = replace,
        #                    sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
        #                      if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
        #                    nodesize = if (!is.null(nodesize)) nodesize[1] else 1,
        #                    maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)
        #
        RF <- ranger(x = obsX,
                     y = obsY,
                     num.trees = ntree,
                     mtry = mtry)

        ## record out-of-bag error (MSE)
        OOBerror[varInd] <- RF$prediction.error

        # save model
        models[[iter + 1]][[varInd]] <- RF

        if (nrow(misX) > 0) { # if the column is not complete
          misY <- predict(RF, misX)$predictions
        } else {
          misY <- c()
        }

      } else {
        obsY <- factor(obsY)
        summarY <- summary(obsY)
        if (length(summarY) == 1) {
          misY <- factor(rep(names(summarY), sum(misi)))
        } else {
          #RF <- randomForest(x = obsX,
          #                   y = obsY,
          #                   ntree = ntree,
          #                   mtry = mtry,
          #                   replace = replace,
          #                   classwt = if (!is.null(classwt)) classwt[[varInd]] else
          #                     rep(1, nlevels(obsY)),
          #                   cutoff = if (!is.null(cutoff)) cutoff[[varInd]] else
          #                     rep(1 / nlevels(obsY), nlevels(obsY)),
          #                   strata = if (!is.null(strata)) strata[[varInd]] else obsY,
          #                   sampsize = if (!is.null(sampsize)) sampsize[[varInd]] else
          #                     if (replace) nrow(obsX) else ceiling(0.632 * nrow(obsX)),
          #                   nodesize = if (!is.null(nodesize)) nodesize[2] else 5,
          #                   maxnodes = if (!is.null(maxnodes)) maxnodes else NULL)

          RF <- ranger(x = obsX,
                       y = obsY,
                       num.trees = ntree,
                       mtry = mtry,
                       class.weights = if (!is.null(class.weights)) class.weights[[varInd]], # TODO: test this and cutoff and strata
                       probability = TRUE)

          # record out-of-bag error (Brier score when probability = TRUE)
          OOBerror[varInd] <- RF$prediction.error

          # save model
          models[[iter + 1]][[varInd]] <- RF

          ## predict missing parts of Y
          #misY <- predict(RF, misX)
          if (nrow(misX) > 0) { # if the column is not complete
            #misY <- predict(RF, misX)$predictions

            # if factor, return factor
            preds <- predict(RF, misX)$predictions
            levels <- colnames(preds)
            misY <- apply(preds, 1, function(x) levels[which.max(x)])

          } else {
            misY <- c()
          }
        }
      }
      ximp[misi, varInd] <- misY

    }

    names(models[[iter + 1]]) <- col_names

    cat('done!\n')

    iter <- iter + 1
    Ximp[[iter]] <- ximp

    t.co2 <- 1
    ## check the difference between iteration steps
    for (t.type in names(convNew)){
      t.ind <- which(varType == t.type)
      if (t.type == 'numeric'){
        convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, t.ind])^2) / sum(ximp[, t.ind]^2)
      } else {
        #dist <- sum(as.character(as.matrix(ximp[, t.ind])) != as.character(as.matrix(ximp.old[, t.ind])))
        #convNew[t.co2] <- dist / (n * sum(varType == 'factor'))

        # TODO: return probabilities
        # TODO: test for binary and categorical

        # gets a factor variable, returns a matrix of binary variables
        make_binary <- function(x) {

          unique_vals <- levels(x)
          x_binary <- matrix(ncol = length(unique_vals), nrow = length(x))

          for (i in 1:length(unique_vals)) {
            x_binary[,i] <- ifelse(x == unique_vals[[i]], 1, 0)
          }

          return(x_binary)

        }

        conv_new_cols <- c()

        for (t in t.ind){

          ximp_old_binary <- make_binary(ximp.old[, t])
          ximp_new_binary <- make_binary(ximp[, t])

          mean_observed <- mean(ximp_old_binary) # old
          bs_baseline <- mean((mean_observed - ximp_old_binary)^2)

          conv_new_cols <- c(conv_new_cols, mean((ximp_new_binary - ximp_old_binary)^2) / bs_baseline)
          # this should be 1 - BSS
        }

        convNew[t.co2] <- mean(conv_new_cols)
      }
      t.co2 <- t.co2 + 1
    }

    ## compute estimated imputation error
    if (!variablewise){
      NRMSE <- sqrt(mean(OOBerror[varType == 'numeric'])/
                      var(as.vector(as.matrix(xmis[, varType == 'numeric'])),
                          na.rm = TRUE))
      PFC <- mean(OOBerror[varType == 'factor'])
      if (k == 1){
        if (unique(varType) == 'numeric'){
          OOBerr <- NRMSE
          names(OOBerr) <- 'NRMSE'
        } else {
          OOBerr <- PFC
          names(OOBerr) <- 'PFC'
        }
      } else {
        OOBerr <- c(NRMSE, PFC)
        names(OOBerr) <- c('NRMSE', 'PFC')
      }
    } else {
      OOBerr <- OOBerror
      names(OOBerr)[varType == 'numeric'] <- 'MSE'
      names(OOBerr)[varType == 'factor'] <- 'PFC'
    }

    if (any(!is.na(xtrue))){
      err <- suppressWarnings(mixError(ximp, xmis, xtrue))
    }

    ## return status output, if desired
    if (verbose){
      delta.start <- proc.time() - t.start
      if (any(!is.na(xtrue))){
        cat("    error(s):", err, "\n")
      }
      cat("    estimated error(s):", OOBerr, "\n")
      cat("    difference(s):", convNew, "\n")
      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }#end while((convNew<convOld)&(iter<maxiter)){

  ## produce output w.r.t. stopping rule
  if (iter == maxiter){
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr)
    } else {
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr, error = err)
    }
  } else {
    if (any(is.na(xtrue))){
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld)
    } else {
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld,
                  error = suppressWarnings(mixError(Ximp[[iter - 1]], xmis, xtrue)))
    }
  }

  # save single initialization as list
  out$init <- var_single_init
  out$models <- models
  out$impute_sequence <- col_names[sort.j]
  out$iter <- iter
  out$maxiter <- maxiter

  class(out) <- 'missForest'
  return(out)
}
