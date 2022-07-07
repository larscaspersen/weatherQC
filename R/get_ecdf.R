#' Helper function for spatial corroboration test
#' 
#' Function calculates for a specified day of the year the empirical cumulative density 
#' function of non-zero precipitation events.
#' 
#' This is a helper function which is mainly used for the spatial corroboration test
#' of precipitation. The function includes precipitation data of a 29 day window centered at
#' the day of interest throughout all observed years. Missing observations and 
#' zer-precipitation observations are discarded. If there are fewer observations than 
#' \code{min_non_zero_days}, then NA is returned instead of the empirical cumulative
#' density function.
#' 
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param doy Day of the year for which the empirical cumulative density function of precipitatio should be calculated
#' @param min_non_zero_days minimum amount of non-zero precipitation observation need 
#' to be present for ecdf calculation, otherwise NA returned
#' @return empirical cumulative density function
#' @seealso \code{\link{ecdf}}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' get_ecdf(weather = target_weather, doy = 1)
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
get_ecdf <- function(weather, doy, min_non_zero_days = 20){
  #get doys of target days
  lim_doy <- doy + c(-14,14)
  
  #adjust for lower lims in old year
  lim_doy <- ifelse(lim_doy <= 0, yes = lim_doy + 365,no = lim_doy)
  
  #adjsut for upper lims in new year
  lim_doy <- ifelse(lim_doy >= 365, yes = lim_doy - 365, no = lim_doy)
  
  
  #if the range of days falls inbetween two years, than different subsetting needed
  if(lim_doy[1] > lim_doy[2]){
    
    target_days <- weather$doy >= lim_doy[1] | weather$doy <= lim_doy[2]
    
  } else {
    
    target_days <- weather$doy >= lim_doy[1] & weather$doy <= lim_doy[2]
  }
  
  #check if at least 20 non_zero values are present
  if(sum(weather$Precip[target_days] > 0, na.rm = T) < min_non_zero_days){
    return(NA)
  }
  
  #take non zero precipitation data from the 29 window over all years, 
  #make it a empirical cumulative distribution function, then determine 
  #the percentile of the valuz 
  return(weather$Precip[target_days] %>%
           na.omit() %>%
           .[.>0] %>%
           ecdf())
  
}