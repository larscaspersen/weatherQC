#' Calculate index of agreement
#' 
#' Function to calculate index of agreement
#' 
#' The index of agreement expresses the ratio of mean square error and the potential error.
#' It expresses both the covariation and the absolute difference of target and auxiliary
#' weather station. The index of agreement can range from 0 to 1, the higher the value
#' the better the agreement of the compared objects. The index of agreement is 
#' sensitive to extreme values. 
#' @param x = target observation
#' @param y = neighbor observation
#' @return index of agreement
#' @examples calc_index_agreement(x = 1:10, y = 2:11)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
calc_index_agreement <- function(x,y){
  return(1 - ((sum(abs(y-x), na.rm = T))/(sum(abs(x - mean(y,na.rm = T)) + abs((y - mean(y, na.rm = T))),na.rm = T) )))
}