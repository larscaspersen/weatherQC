#' Retrieves closest value
#' 
#' Function finds closest value of y for x
#' 
#' This is a helper function needed for the spatial consistency test. It simply 
#' returns the closest value in y for the value of x. If there are two or more
#' values in y which are the closest to x, the first one is returned. In case x 
#' or y only contains NA, the function returns NA instead.
#' 
#' @param x numerical value
#' @param y vector, for which the closest value compared to x is searched
#' @return value in \code{y} which is the closest to \code{x}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples get_closest_y(x = 1, y = c(0,2,3))
get_closest_y <-  function(x,y,i){
  #if x is NA or all of y, then return na
  if(is.na(x)) return(NA)
  if(all(is.na(y[(i-1):(i+1)]))) return(NA)
  
  #find closest y to x, if two are similar close then take the first
  return(y[(i-1):(i+1)][which(min(abs(y[(i-1):(i+1)] - x), na.rm =T) == (abs(y[(i-1):(i+1)] - x)))[1]])
}