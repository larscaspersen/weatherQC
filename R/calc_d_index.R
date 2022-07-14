#' Calculate the refined index of agreement
#' 
#' This function calculates the refined index of agreement \insertCite{willmott_refined_2012}{weatherQC}, 
#' which is an evaluation metric especially suited for precipitation occurrence 
#' in aerid regions \insertCite{aguilera_estimating_2020}{weatherQC}.
#' 
#' The refined index of agreement can range from -1 to 1, where a value close to one indicates good
#' model performance to predict precipitation occurrence. It is said to approach
#' values of 1 slowly, so it is also suited to discriminated well performing
#' models. 
#' 
#' 
#' @param predicted numeric vector containing predicted values
#' @param observed numeric vector containing observed values, should be of same
#' length as \code{predicted}
#' @return numeric outcome of the refined index of agreement
#' @examples 
#' calc_d_index(predicted = c(rep(0,5), rep(1, 5)), 
#' observed = c(0,0, 0, 1, 1, 1, 1, 0, 0, 0))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @references
#' \insertAllCited{}
#' @export
calc_d_index <- function(predicted, observed){
  
  mean_obs <- mean(observed)
  n <- length(predicted)
  
  if(sum(abs(predicted - observed)) <= 2*sum(abs(observed - mean_obs))){
    
    return(1 - (sum(abs(predicted - observed)) / (2*sum(abs(observed - mean_obs)))))
    
  } else {
    return(((2*sum(abs(observed - mean_obs))) / (sum(abs(predicted - observed)))) - 1)
  }
}