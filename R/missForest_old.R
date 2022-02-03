#' Title
#'
#' @param xmis foo
#' @param maxiter foo
#' @param ntree foo
#' @param variablewise foo
#' @param decreasing foo
#' @param verbose foo
#' @param mtry foo
#' @param replace foo
#' @param class.weights foo
#' @param cutoff foo
#' @param strata foo
#' @param sample.fraction foo
#' @param maxnodes foo
#' @param xtrue foo
#'
#' @return foo
#' @noRd

missForest_old <- function(xmis, maxiter = 10, ntree = 500, variablewise = FALSE,
                       decreasing = FALSE, verbose = FALSE,
                       mtry = floor(sqrt(ncol(xmis))), replace = TRUE,
                       class.weights = NULL, cutoff = NULL, strata = NULL,
                       sample.fraction = NULL,
                       #nodesize = NULL,
                       maxnodes = NULL,
                       xtrue = NA)
{ ## ----------------------------------------------------------------------
  ## Arguments:
  ## xmis         = data matrix with missing values
  ## maxiter      = stop after how many iterations (default = 10)
  ## ntree        = how many trees are grown in the forest (default = 100)
  ## variablewise = (boolean) return OOB errors for each variable separately
  ## decreasing   = (boolean) if TRUE the columns are sorted with decreasing
  ##                amount of missing values
  ## verbose      = (boolean) if TRUE then missForest returns error estimates,
  ##                runtime and if available true error during iterations
  ## mtry         = how many variables should be tried randomly at each node
  ## replace      = (boolean) if TRUE bootstrap sampling (with replacements)
  ##                is performed, else subsampling (without replacements)
  ## class.weights      = list of priors of the classes in the categorical variables
  ## cutoff       = list of class cutoffs for each categorical variable
  ## strata       = list of (factor) variables used for stratified sampling
  ## sample.fraction     = list of size(s) of sample.fraction to draw
  ## nodesize     = minimum size of terminal nodes, vector of length 2, with
  ##                number for continuous variables in the first entry and
  ##                number for categorical variables in the second entry
  ## maxnodes     = maximum number of terminal nodes for individual trees
  ## xtrue        = complete data matrix
  ##
  ## ----------------------------------------------------------------------
  ## Author: Daniel Stekhoven, stekhoven@nexus.ethz.ch

  ## stop in case of wrong inputs passed to randomForest
  n <- nrow(xmis)
  p <- ncol(xmis)
  col_names <- colnames(xmis)

  if (!is.null(class.weights))
    stopifnot(length(class.weights) == p, typeof(class.weights) == 'list')
  if (!is.null(cutoff))
    stopifnot(length(cutoff) == p, typeof(cutoff) == 'list')
  if (!is.null(strata))
    stopifnot(length(strata) == p, typeof(strata) == 'list')
  #if (!is.null(nodesize))
  #  stopifnot(length(nodesize) == 2)

  ## remove completely missing variables
  if (any(apply(is.na(xmis), 2, sum) == n)){
    indCmis <- which(apply(is.na(xmis), 2, sum) == n)
    xmis <- xmis[,-indCmis]
    p <- ncol(xmis)
    cat('  removed variable(s)', indCmis,
        'due to the missingness of all entries\n')
  }

  ## perform initial S.W.A.G. on xmis (mean imputation)
  ximp <- xmis
  varType <- character(p)
  var_single_init <- list()
  for (t.co in 1:p) {
    if (is.numeric(xmis[[t.co]])) {
      varType[t.co] <- 'numeric'
      # keep mean
      mean_col <- mean(xmis[,t.co], na.rm = TRUE)
      var_single_init[[t.co]] <- mean_col

      ximp[is.na(xmis[,t.co]),t.co] <- mean_col
      next()
    }
    if (is.factor(xmis[[t.co]])) {
      varType[t.co] <- 'factor'
      ## take the level which is more 'likely' (majority vote)
      max.level <- max(table(ximp[[t.co]]))
      ## if there are several classes which are major, sample one at random
      class.assign <- sample(names(which(max.level == summary(ximp[[t.co]]))), 1)
      ## it shouldn't be the NA class
      if (class.assign != "NA's") {
        # keep mode
        var_single_init[[t.co]] <- class.assign

        ximp[is.na(xmis[[t.co]]),t.co] <- class.assign
      } else {
        while (class.assign == "NA's") {
          class.assign <- sample(names(which(max.level ==
                                               summary(ximp[[t.co]]))), 1)
        }
        # keep mode
        var_single_init[[t.co]] <- class.assign

        ximp[is.na(xmis[[t.co]]),t.co] <- class.assign
      }
      next()
    }
    stop(sprintf('column %s must be factor or numeric, is %s', names(xmis)[t.co], class(xmis[[t.co]])))
  }

  # keep column names
  names(var_single_init) <- col_names

  ## extract missingness pattern
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
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & (iter < maxiter)
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

    if (verbose) cat("  missForest iteration", iter+1, "in progress...")

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
                     mtry = mtry,
                     replace = replace,
                     sample.fraction = if (!is.null(sample.fraction)) sample.fraction else
                       if (replace) 1 else 0.632)
        #min.node.size = if (!is.null(nodesize)) nodesize[1] else 1,
        #max.depth = if (!is.null(maxnodes)) maxnodes else NULL) # unsure of this, to check
        ## record out-of-bag error
        #OOBerror[varInd] <- RF$mse[ntree]
        OOBerror[varInd] <- RF$prediction.error
        # Overall out of bag prediction error. For classification this is the fraction of missclassified samples,
        # for probability estimation the Brier score, for regression the mean squared error
        # and for survival one minus Harrell's C-index.

        # save model
        models[[iter + 1]][[varInd]] <- RF

        # misY <- predict(RF, misX)
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
                       replace = replace,
                       class.weights = if (!is.null(class.weights)) class.weights[[varInd]], # TODO: test this and cutoff and strata
                       sample.fraction = if (!is.null(sample.fraction)) sample.fraction else
                         if (replace) 1 else 0.632)
          #min.node.size = if (!is.null(nodesize)) nodesize[2] else 5,
          #max.depth = if (!is.null(maxnodes)) maxnodes else NULL)
          ## record out-of-bag error
          #OOBerror[varInd] <- RF$err.rate[[ntree, 1]]
          OOBerror[varInd] <- RF$prediction.error

          # save model
          models[[iter + 1]][[varInd]] <- RF

          ## predict missing parts of Y
          #misY <- predict(RF, misX)
          if (nrow(misX) > 0) { # if the column is not complete
            misY <- predict(RF, misX)$predictions
          } else {
            misY <- c()
          }
        }
      }
      ximp[misi, varInd] <- misY

    }

    names(models[[iter + 1]]) <- col_names

    if (verbose) cat('done!\n')

    iter <- iter + 1
    Ximp[[iter]] <- ximp

    t.co2 <- 1
    ## check the difference between iteration steps
    for (t.type in names(convNew)){
      t.ind <- which(varType == t.type)
      if (t.type == 'numeric'){
        convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, t.ind])^2) / sum(ximp[, t.ind]^2)
      } else {
        dist <- sum(as.character(as.matrix(ximp[, t.ind])) != as.character(as.matrix(ximp.old[, t.ind])))
        convNew[t.co2] <- dist / (n * sum(varType == 'factor'))
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
