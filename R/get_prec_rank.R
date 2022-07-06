#' Calculate the climatological percentile of a certain precipitation event
#' 
#' Function calculates for a given precipitation event the corresponding climatological
#' (floored) percentile rank.
#' 
#' This is a helper function which is mainly used for the spatial corroboration test
#' of precipitation. The function is wrapper of the \code{get_ecdf()} function. 
#' It includes precipitation data of a 29 day window centered at
#' the day of interest throughout all observed years and calculates the empirical
#' cumulative density function (ecdf). Missing observations and 
#' zer-precipitation observations are discarded. If there are fewer observations than 
#' \code{min_non_zero_days}, then NA is returned instead of the ecdf. Then the 
#' precipitation event of interest is inserted in the ecdf and the corresponding 
#' percentile rank is calculated. This is done for each precipitation event.
#' 
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param min_non_zero_days minimum amount of non-zero precipitation observation need 
#' to be present for ecdf calculation, otherwise NA returned
#' @return vector of same length as rows in weather with percent percentile calculated
#' as: \code{floor(percentile * 100)} 
#' @seealso \code{\link{ecdf}}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' get_prec_rank(weather = weather)
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
get_prec_rank <- function(weather,min_non_zero_days = 20){
  
  #add date and doy
  weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep ='-'),
                          format = '%Y-%m-%d')
  weather$doy <- lubridate::yday(weather$Date)
  
  
  
  ecdf_list <- map(unique(weather$doy), ~ get_ecdf(weather = weather, doy = .x, min_non_zero_days = min_non_zero_days))
  
  
  #somehow this only works if I assign one of the functions to y
  y <- ecdf_list[[1]]
  
  return(map2(split(weather,f = weather$doy), ecdf_list, function(x,y) floor(sapply(x$Precip,y) * 100)) %>%
           unsplit(weather$doy))
  
}
