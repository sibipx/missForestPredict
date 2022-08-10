#' Calculates convergence based on NMSE
#'
#' Calculates convergence based on NMSE. TODO: blabla
#'
#' @param OOB_err dataframe containing OOB errors for each iteration.
#' @param OOB_weights vector of weights in the same format as for the \code{missForest} function.
#' @param ximp_new imputed dataframe of current iteration
#' @param ximp_old imputed dataframe of previous iteration
#' @param xmis the original dataframe containing missing values.
#'
#' @return A list with elements
#'     \item{\code{converged}}{boolean indicating if the algorithm has converged (TRUE) or not (FALSE)}
#'     \item{\code{measure_old}}{old measure: TODO: will this work?}
#'     \item{\code{measure_new}}{new measure: TODO: will this work?}
#' @export

calculate_convergence <- function(OOB_err, OOB_weights,
                                  ximp_new, ximp_old,
                                  xmis){

  iter <- max(OOB_err$iteration[!is.na(OOB_err$NMSE)])

  NMSE_err_new <- weighted.mean(OOB_err[OOB_err$iteration == iter,"NMSE"],
                                w = OOB_weights)
  if (iter == 1) {
    # TODO: isn't this always 1?
    #NMSE_err_old <- weighted.mean(rep(1, length(impute_sequence)), w = OOB_weights[impute_sequence])
    NMSE_err_old <- 1
  } else {
    NMSE_err_old <- weighted.mean(OOB_err[OOB_err$iteration == iter - 1,"NMSE"],
                                  w = OOB_weights)
  }

  converged <- NMSE_err_new >= NMSE_err_old

  return(list(converged = converged,
              measure_old = NMSE_err_old,
              measure_new = NMSE_err_new))

}
