#' Calculate hit score
#' 
#' This function calculates the hit score, which is an evaluation metric
#' especially suited for precipitation.
#' 
#' The hitscore is the mean of correctly identified precipitation events and 
#' correctly identified dry-events \insertCite{simolo_improving_2010,oriani_missing_2020}{weatherQC}.
#' The function does not evaluate the imputed precipitation amount but only the 
#' precipitation occurrence.
#' 
#' @param predicted numeric vector containing predicted values
#' @param observed numeric vector containing observed values, should be of same
#' length as \code{predicted}
#' @return numeric outcome of the hit score
#' @examples calc_hit_score(predicted = c(rep(0,5), rep(1, 5)), 
#' observed = c(0,0, 0, 1, 1, 1, 1, 0, 0, 0))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @references
#' \insertAllCited{}
#' @export
calc_hit_score <- function(observed, predicted){
  
  #transform precipitation data to occurence data
  observed_occ <-  observed > 0
  predicted_occ <- predicted > 0
  
  #get share of correctly identified wet days to all predicted days of precipitation
  no_wet_pred <- sum(predicted_occ)
  if(no_wet_pred == 0){
    stop("The model only predicted dry days and never a rainy day. Seems odd. Also this forces to devide by 0, so no hit fraction could be calculated")
  }
  no_correct_wet <- sum(predicted_occ == T & observed_occ == T)
  h1 <- no_correct_wet / no_wet_pred
  #if it predicts wet, how often is it really wet?
  
  #get share of correctly identified dry days of all predicted dry days
  no_dry_pred <- sum(!predicted_occ)
  if(no_dry_pred == 0){
    stop("The model only predicted rain and never a dry day. Seems odd. Also this forces to devide by 0, so no hit fraction could be calculated")
  }
  no_correct_dry <- sum(predicted_occ == F & observed_occ == F)
  h0 <- no_correct_dry / no_dry_pred
  #if it predicts dry, how often is it really dry?
  
  return((h0 + h1)/2)
  
}