#' Calculates convergence based on NMSE
#'
#' Calculates convergence based on NMSE.
#'
#' @param err dataframe containing OOB or apparent errors for each iteration.
#' @param weights vector of weights in the same format as for the \code{missForest} function.
#'
#' @return A list with elements
#'     \item{\code{converged}}{boolean indicating if the algorithm has converged (TRUE) or not (FALSE)}
#'     \item{\code{measure_old}}{old measure: TODO: will this work?}
#'     \item{\code{measure_new}}{new measure: TODO: will this work?}
#' @export

calculate_convergence <- function(err, weights){

  iter <- max(err$iteration[!is.na(err$NMSE)])

  NMSE_err_new <- weighted.mean(err[err$iteration == iter,"NMSE"],
                                w = weights)
  if (iter == 1) {
    # TODO: isn't this always 1?
    #NMSE_err_old <- weighted.mean(rep(1, length(impute_sequence)), w = weights[impute_sequence])
    NMSE_err_old <- 1
  } else {
    NMSE_err_old <- weighted.mean(err[err$iteration == iter - 1,"NMSE"],
                                  w = weights)
  }

  converged <- NMSE_err_new >= NMSE_err_old

  return(list(converged = converged,
              measure_old = NMSE_err_old,
              measure_new = NMSE_err_new))

}
