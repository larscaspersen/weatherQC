#' Calculate mean absolute error
#' 
#' This function calculates the mean absolute error for predicted and 
#' observed values.
#' 
#' The lower the MAE the better, it ranges from 0 to positive infinity. It is
#' interpreted to be the mean distance of prediction to observation, with 
#' equal weight for each residual. It is said to perform better if the error
#' is uniformly distributed.
#' 
#' @param predicted numeric vector containing predicted values
#' @param observed numeric vector containing observed values, should be of same
#' length as \code{predicted}
#' @param Boolean parameter indicating whether NA values should be removed before the analysis
#' @return numeric value of mean absolute error
#' @examples calc_MAE(predicted = 1:10, observed = 2:11)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
calc_MAE <- function(predicted, observed, na.rm = FALSE){
  if (!na.rm) 
    if (!(length(which(is.na(predicted))) + length(which(is.na(observed)))) == 
        0) 
      stop("Datasets include NA values. This may indicate a serious prediction problem. To override this error, set na.rm=TRUE.")
  
  mae <- sum(abs(observed - predicted)) / length(which(!is.na(observed - predicted)))
  return(mae)
}