#' Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Brier Score

BS <- function (probabilities, y) {
  mean(rowSums((probabilities - y)^2))
}

#' Normalized Brier Score
#'
#' @param probabilities matrix of predicted probabilities for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Normalized Brier Score

BSnorm <- function (probabilities, y) {
  #BS_reference <- BS(colMeans(y), y) # refrence is a "no skill learner" that predicts the class prevalence
  BS_reference <- BS(matrix(rep(colMeans(y), nrow(y)), nrow = nrow(y), byrow = TRUE), y)
  BS(probabilities, y) / BS_reference
}

#' Mean Square Error - MSE
#'
#' @param preds vector of predictions
#' @param y vector of true values for y
#'
#' @return Mean Square Error - MSE

mse <- function (preds, y) {
  mean((y-preds)^2)
}

#' Normalized Mean Square Error - NMSE
#'
#' @param preds vector of predictions
#' @param y vector of true values for y
#'
#' @return Normalized Mean Square Error - NMSE

nmse <- function (preds, y) {
  m <- mean(y)
  mse(preds, y) / mean((y-m)^2)
}

#' Missclassification error rate - ER
#'
#' @param probabilities matrix of class predictions for each class (classes in columns, observations in rows)
#' @param y matrix of true values for y; each column contains a class ordered in the same order as probabilities
#'
#' @return Missclassification error rate

mer <- function (prediction, y) {
  mean(prediction != y)
}




