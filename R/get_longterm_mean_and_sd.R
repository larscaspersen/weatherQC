#' Helper function for the climate outlier test
#' 
#' This function calculates longterm mean and standard deviation of temperature data
#' for a certain day of interest.
#' 
#' The function usesa a 15-day window centered at the day of interest and uses
#' each observation throughout all years within that time-window to calculate mean
#' and standard deviation.
#' 
#' This is a helper function for the higher-level function \code{\link{perform_climate_outlier_check}}
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable 
#' in weather
#' @return data.frame with columns c(doy, mean, sd)
#' @seealso \code{\link{perform_climate_outlier_check}}
#' @example get_longterm_mean_and_sd(weather = weather, variable = "Tmin", doy = 1)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
get_longterm_mean_and_sd <- function(weather, variable, doy){
  #get doys of target days
  lim_doy <- doy + c(-7,7)
  
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
  
  
  
  return(data.frame(doy = doy,
                    mean = mean(weather[[variable]][target_days], na.rm =T), 
                    sd = sd(weather[[variable]][target_days], na.rm =T)))
  
}