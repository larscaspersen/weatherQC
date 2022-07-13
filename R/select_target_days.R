#' Select auxiliary weather station observation for spatial consistency test
#' 
#' Selects observations of neighboring weather stations to be included in target - 
#' neighbour weather station linear regression calculation.
#' 
#' Retrieves observations of weather station based on defined time period. In case
#' parts of the requested period are not present in data.frame, function returns
#' NA for these days. This function is a helper function for the spatial consistency
#' test of temperature data.
#' @param df data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable column name of \code{df} for which data should be retrieved
#' @param period_start in date format, indicates start of the period of interest
#' @param period_end in date format, indicates end of the period of interest
#' @return numerical vector of the same length as \code{period_start:period_end}
#' @examples select_target_days(weather = target_weather, variable = "Tmin", 
#' period_start = as.Date("1991-12-17", format = "%Y-%m-%d"), 
#' period_end = as.Date("1991-02-15", format = "%y-%m-%d"))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
select_target_days <- function(df, variable, period_start, period_end){
  
  #if not present, add Date column
  df$Date <- as.Date(paste(df$Year, df$Month, df$Day, sep = '-'), format = '%Y-%m-%d')
  
  #in case every day of the target period is also present in the weather dataframe, then simply reutrn everything
  if(all(period_start:period_end %in% df$Date)){
    return(dplyr::pull(df[df$Date >= period_start & df$Date <= period_end,variable]))
  } else{
    #make a placeholder for the selected days
    re_vec <- rep(NA, as.numeric(period_end-period_start +1))
    
    #change days which are present in the 
    re_vec[period_start:period_end %in% df$Date] <- dplyr::pull(df[df$Date >= period_start & df$Date <= period_end,variable])
    
    return(re_vec)
    
  }
  
}