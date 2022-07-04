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
#' @param y = neighbour observation
#' @return index of agreemenmt
#' @example calc_index_agreement(x = 1:10, y = c(1:3, 6, 5:7, 7,9:10))
calc_index_agreement <- function(x,y){
  return(1 - ((sum(abs(y-x), na.rm = T))/(sum(abs(x - mean(y,na.rm = T)) + abs((y - mean(y, na.rm = T))),na.rm = T) )))
}