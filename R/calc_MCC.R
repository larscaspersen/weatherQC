#' Calculate Matthews Correlation Coefficient
#' 
#' This function calculates Matthews correlation coefficient (MCC), which is an evaluation metric
#' especially suited for unbalanced classification problems (like precipitation
#' occurrence)
#' 
#' The MCC can range from -1 to 1, where a value close to one indicates good
#' performance. It handles imbalanced groups better than other popular
#' classification problem metrics like F1 or accuracy score.
#' 
#' @param predicted numeric vector containing predicted values
#' @param observed numeric vector containing observed values, should be of same
#' length as \code{predicted}
#' @return numeric outcome of the MCC
#' @examples calc_MCC(predicted = c(rep(0,5), rep(1, 5)), 
#' observed = c(0,0, 0, 1, 1, 1, 1, 0, 0, 0))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
calc_MCC <- function(observed, predicted){
  
  #transform precipitation data to occurence data
  observed_occ <-  observed > 0
  predicted_occ <- predicted > 0
  
  #get confusuin matrix elements
  TP <- sum(observed_occ == T & predicted_occ == T) / length(observed_occ)
  TN <- sum(observed_occ == F & predicted_occ == F) / length(observed_occ)
  FN <- sum(observed_occ == T & predicted_occ == T) / length(observed_occ)
  FP <- sum(observed_occ == F & predicted_occ == T) / length(observed_occ)
  
  #get number of total predicted/observed wet days / dry days
  share_wet_obs <- sum(observed_occ == T) / length(observed_occ)
  share_wet_pred <- sum(predicted_occ == T) / length(observed_occ)
  share_dry_obs <- sum(observed_occ == F) / length(observed_occ)
  share_dry_pred <- sum(predicted_occ == F) / length(observed_occ)
  
  #calculate mathews correlation coefficient
  return(((TP * TN) - (FP * FN)) / sqrt(share_dry_obs * share_wet_obs * share_dry_pred * share_wet_pred))
}