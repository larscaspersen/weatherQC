#' Calculate S-index
#' 
#' This function calculates the S-index for predicted and observed data.
#' 
#' The S-index can range from 0 to 1, with higher values indicating better
#' model performance. It is said to express the agreement percentage 
#' of predicted and observed values. It can detect proportional difference
#' in observed and predicted means and is sensitive to extreme values.
#' 
#' @param predicted numeric vector containing predicted values
#' @param observed numeric vector containing observed values, should be of same
#' length as \code{predicted}
#' @return numeric outcome of the S-index
#' @examples calc_S_index(predicted = 1:10, observed = 2:11)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
calc_S_index <- function(predicted, observed){
  pred_mean <- mean(predicted, na.rm = T)
  obs_mean <- mean(observed, na.rm = T)
  
  return((sum((predicted - pred_mean) * (observed - obs_mean)) / sqrt(sum((predicted - pred_mean)^2) * sum((observed - obs_mean)^2)))^2)
}