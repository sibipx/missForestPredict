#' Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Brier Score
#' @keywords internal
#' @noRd

BS <- function (probabilities, y) {
  mean(rowSums((probabilities - y)^2))
}

#' Normalized Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Normalized Brier Score
#' @keywords internal
#' @noRd

BSnorm <- function (probabilities, y) {
  # reference is a "no skill learner" that predicts the class prevalence
  BS_reference <- BS(matrix(rep(colMeans(y), nrow(y)), nrow = nrow(y), byrow = TRUE), y)
  if (BS_reference == 0){ # avoid division by 0
    return(0)
  } else {
    return(BS(probabilities, y) / BS_reference)
  }
}

#' Mean Square Error - MSE
#'
#' @param preds vector of predictions
#' @param y vector of true values for y
#'
#' @return Mean Square Error - MSE
#' @keywords internal
#' @noRd

mse <- function (preds, y) {
  mean((y-preds)^2)
}

#' Normalized Mean Square Error - NMSE
#'
#' @param preds vector of predictions
#' @param y vector of true values for y
#'
#' @return Normalized Mean Square Error - NMSE
#' @keywords internal
#' @noRd

nmse <- function (preds, y) {
  m <- mean(y)
  mse(preds, y) / mean((y-m)^2)
}

#' Normalized Root Mean Square Error - NRMSE
#'
#' @param ximp dataframe with imputed values
#' @param xmis original dataframe with missing values
#' @param xtrue original complete dataframe
#'
#' @return Normalized Root Mean Square Error - NMSE
#' @keywords internal
#' @noRd

nrmse <- function(ximp, xmis, xtrue){
  mis <- is.na(xmis)
  sqrt(mean((ximp[mis] - xtrue[mis])^{2}) / var(xtrue[mis]))
}

#' Missclassification error rate - ER
#'
#' @param prediction matrix of class predictions for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Missclassification error rate
#' @importFrom stats var
#' @keywords internal
#' @noRd

mer <- function (prediction, y) {
  mean(prediction != y)
}




