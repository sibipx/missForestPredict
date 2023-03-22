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
#' @param prediction vector of class predictions
#' @param y vector of true values for y
#'
#' @return Missclassification error rate
#' @keywords internal
#' @noRd

mer <- function (prediction, y) {
  prediction <- as.character(prediction)
  y <- as.character(y)

  mean(prediction != y)
}

#' Macroaveraged F1 score
#'
#' @param prediction vector of class predictions
#' @param y vector of true values
#'
#' @return Macroaveraged F1 score
#' @keywords internal
#' @noRd

macro_F1 <- function(prediction, y) {

  F1 <- F1_score_per_class(prediction, y)

  # only for multi-class
  if (length(F1) <= 2) return(NA_real_)

  macro_F1 <- ifelse(length(F1) <= 2, NA_real_, mean(F1))

  return(macro_F1)
}

#' F1 score for each class
#'
#' @param prediction vector of class predictions
#' @param y vector of true values
#'
#' @return F1 score for each class
#' @keywords internal
#' @noRd

F1_score_per_class <- function (prediction, y) {

  prediction <- factor(as.character(prediction), levels=unique(as.character(y)))
  y  <- factor(as.character(y), levels=unique(as.character(y)))

  cm <- as.matrix(table(y, prediction))

  precision <- diag(cm) / colSums(cm)
  recall <- diag(cm) / rowSums(cm)
  F1_score <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

  # if denominator is 0 make 0
  F1_score[is.na(F1_score)] <- 0

  return(F1_score)

}

#' F1 score for two class variable
#'
#' @param prediction vector of class predictions
#' @param y vector of true values
#' @param positive_class name of positive clas; if NULL the lowest prevalence class is considered positive class
#'
#' @return F1 score for two class variable
#' @keywords internal
#' @noRd

F1_score <- function (prediction, y, positive_class = NULL){

  F1 <- F1_score_per_class(prediction, y)

  # only for 2 class
  if (length(F1) > 2) return(NA_real_)

  if (is.null(positive_class))
    positive_class <- names(which.min(table(as.character(y))))

  F1_score <- F1[[positive_class]]

  return(F1_score)

}

