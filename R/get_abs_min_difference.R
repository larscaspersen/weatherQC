#' Calculate absolute minimum difference of target value to neighboring stations
#' 
#' This function is a simple helper function for the spatial corroboration test.
#' 
#' The function takes a value of the weather variable of interest and calculates
#' the absolute minimum difference of the target value to the observations
#' of the neighboring stations in a time window of three days centered at the
#' day of interest. Special case of precipitation: if x is not strictly smaller or
#' larger than all of the neighboring values within the three day window, then 
#' 0 is returned instead of the minimum absolute difference. In case x is NA, all the
#' neighboring values are NA or if the target_date is not covered by any of the neighboring
#' stations NA is returned instead of the minimum absolute difference
#' 
#' @param x numeric value for which the minimum absolute difference should be 
#' calculated
#' @param target_date Date when x was observed
#' @param variable column name, x was taken from. Should be the same name the
#' corresponding observations in aux_list are stored
#' @param aux_list list of data.frames with neighboring weather station observations.
#' Need contain columns \code("Date") and a column with the same name is indicated
#' in parameter \code{variable}
#' @return numeric value with minimum absolute difference of target and neighboring 
#' stations
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' get_abs_min_difference(x = weather$Precip[1], target_date = weather$Date[1],
#' variable = "Precip", aux_list = aux_list)
get_abs_min_difference <- function(x, target_date, variable, aux_list){
  
  if(is.na(x) == TRUE){
    return(NA)
  }

  
  #extract values from aux station with +-1 day
  int <- map(aux_list, function(x){
    x %>%
      filter(Date >= (target_date - 1) & Date <= (target_date + 1)) %>%
      .[[variable]]
  }) %>%
    unlist() %>%
    unname()
  
  #check if there is at least one value of non NA. otherwise return NA,
  #in case no neighbouring station covered the period of interest also return NA
  if(length(int) > 0){
    if(all(is.na(int))){
      return(NA)
    }
  } else{
    return(NA)
  }

  
  #if the value is not the highest or the lowest, then there is no need to carry out 
  #the corrobation test
  
  if(variable == 'Precip'){
    #in case x isn't the largest or the smallest, return zero
    if(all(x > int, na.rm = T) | all(x < int, na.rm = T) == F){
      return(0)
    } else{
    #otherwise calculate absolute minimum difference
      return(min(abs(x - int), na.rm =T))
    }
  } else{
    return(min(abs(x - int), na.rm = T))
  }
}
