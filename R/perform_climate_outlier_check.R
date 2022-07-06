#' Test weather data for climatological outliers
#' 
#' The function takes weather data, calculates long term mean (temperature) or 
#' percentiles (precipitation) and compares daily weather observations with it.
#' 
#' In case of temperature long-term mean and standard deviation are calculated using
#' a 15 day-window centered at the day of interest. All values throughout the observed
#' years withing that time-window are included in the calculation. In case of precipitation
#' the percentiles of non-zero precipitation events are calculated using the function
#' \code{\link{get_each_day_precipitation_percentile}}
#' 
#' Flags are raised if the standardized residual of temperature exceeds a pre-defined
#' threshold (by default 6). Precipitation flags are raised if the observation
#' exceeds the a factor (which changes for freezing conditions) times the percentile 
#' (by default the 95\% percentile is used).
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 4 "Outlier checks".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable 
#' in weather
#' @param max_temperature_z maximum size of standardized resiudla of temperature value
#' to climatological mean for the day of interest
#' @param max_prec_threshold factor for test: \code{precipitation >= max_prec_threshold *
#' precipitation percentile} under non-freezing conditions
#' @param max_prec_threshold_freezing factor for test: \code{precipitation >= max_prec_threshold *
#' precipitation percentile}  under freezing-conditions
#' @param prec_percentile precipitation percentile used for the comparison
#' @return Logical vector of same length as rows in \code{weather}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test.
#' @examples perform_climate_outlier_check(weather = weather, variable = "Tmin")
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
perform_climate_outlier_check <- function(weather, variable, 
                                          max_temperature_z = 6, 
                                          max_prec_threshold = 9, 
                                          max_prec_threshold_freezing = 5,
                                          prec_percentile = 0.95){
  
  if(variable %in% c('Tmin', 'Tmax')){
    
    #calculate longt term mean and sd of temperature for each day of the year for a 15 day window centered at day of interest
    clim_df <- map(unique(weather$doy), ~ get_longterm_mean_and_sd(weather = weather, variable = variable, doy = .x)) %>%
      bind_rows()
    
    #normalise temperature data
    
    weather <- merge(weather, clim_df, by = 'doy') %>%
      arrange(Date)
    
    clim_outlier <- abs((weather[,variable] - weather$mean) / weather$sd) > max_temperature_z
    
    clim_outlier <- replace_na(data = clim_outlier, replace = FALSE)
    
    return(clim_outlier)
  } else if(variable == 'Precip'){
    
    weather <- map(unique(weather$doy), ~ get_clim_percentiles_prec(weather = weather, 
                                                                    doy = .x, probs = prec_percentile)) %>%
      unlist() %>%
      data.frame(doy = unique(weather$doy), percentile = .) %>%
      merge.data.frame(weather, ., by = 'doy') %>%
      arrange(Date)
    
    #in case precipitation happening at freezing temperatures, choose a lower threshold
    clim_outlier <- weather[,variable] >= ifelse((weather$Tmax + weather$Tmin) / 2 > 0, yes = weather$percentile * max_prec_threshold, 
                                                 no = weather$percentile * max_prec_threshold_freezing)
    
    #change na to false
    clim_outlier <- replace_na(data = clim_outlier, replace = FALSE)
    
    return(clim_outlier)
  }
}