#' Calculate skill score
#' 
#' This function calculates the skill score for predicted and observed data.
#' 
#' The skill score can range from negative infinity to 1, with higher values 
#' indicating better model performance. A value lower than 0 indicates that on average the
#' difference of observation to its mean is smaller than the difference of predicted
#' value to observed value. Or to put it simply, always using the observed mean
#' instead of using the model results to less error. A value above 0 indicates
#' a benefit in using the model.
#' 
#' @param predicted numeric vector containing predicted values
#' @param observed numeric vector containing observed values, should be of same
#' length as \code{predicted}
#' @return numeric outcome of the skill score
#' @examples calc_skill_score(predicted = 1:10, observed = 2:11)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
calc_skill_score <- function(predicted, observed){
  
  mse_1 <- sum((predicted - observed)^2) / length(which(!is.na(observed - predicted)))
  mse_2 <- sum((mean(observed, na.rm = T) - observed)^2) / length(which(!is.na(observed - predicted)))
  
  return(1 - (mse_1 / mse_2))
}