#' calculate percentiles for each day of the year
#' 
#' This function calculates climatological precipitation percentiles for each
#' day of the year.
#' 
#' This is a wrapper function of \code{\link{get_clim_percentiles_prec}}.
#' The function includes precipitation data of a 29 day window centered at
#' the day of interest throughout all observed years. Missing observations and 
#' zer-precipitation observations are discarded. If there are fewer observations than 
#' \code{min_non_zero_days}, then NA is returned instead of the empirical cumulative
#' density function.
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param probs numeric vector, indicating the percent percentiles that sould be 
#' calculated
#' @param min_non_zero_days minimum amount of non-zero precipitation observation need 
#' to be present for ecdf calculation, otherwise NA returned
#' @return data.frame with columns c("doy", probs) and 366 rows, one for each
#' day of the year (leap years are included). In the columns of probs the
#' respective percentiles are included. If the percentile calculation failed
#' because of the lack of non-zero data, then the row will contain NA instead
#' @examples get_each_day_precipitation_percentile(weather = target_weather)
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
get_each_day_precipitation_percentile <- function(weather, probs = c(.3, .5, .7, .9),
                                                  min_non_zero_days = 20){
  
  #incase there is no doy in weather, add it
  if('doy' %in% names(weather) == F){
    #in case no date in weather add it too
    if('Date' %in% names(weather) == F){
      weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                              format = '%Y-%m-%d')
    }
    weather$doy <- lubridate::yday(weather$Date)
  }
  
  names <- c('doy', paste0(probs * 100, '%'))
  
  purrr::map(unique(weather$doy), ~ get_clim_percentiles_prec(weather = weather, 
                                                       doy = .x,
                                                       probs = probs, 
                                                       min_non_zero_days = min_non_zero_days)) %>%
    do.call(rbind, .data) %>%
    data.frame(doy = unique(weather$doy), .data) %>%
    `colnames<-`(names)
}