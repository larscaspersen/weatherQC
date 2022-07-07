#' Intermediate function for frequent value test
#' 
#' Depending on the amount of repetitions the function compares the frequent 
#' precipitation value with a corresponding climatological percentile of precipitation
#' for that day of interest.
#' 
#' The more often the value was observed within a 10 day window, the lower the
#' percentile used for the test. 9 or 10 observations: 30\%, 8 observations: 50\%,
#' 7 observations: 70\%, 5 or 6 observations: 90\%. Climatological precipitation
#' percentiles are calculated using \code{\link{get_clim_percentiles_prec}}
#'  
#' @param val precipitation value which was frequently observed in the weather data
#' @param val_rep how often val was observed in a 10 day window
#' @param doy day of the year the value was observed
#' @param percentile_df data.frame containing the climatological precipitation
#' percentiles for the corresponding day of the year
#' @return logical, if the frequent value is larger than the corresponding value in 
#' \code{percentile_df}, then it returns TRUE, otherwise FALSE 
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' #this example is just hypothetical
#' percentile_df <- get_each_day_precipitation_percentile(weather = target_weather)
#' helper_check_frequent_val(val = target_weather$Precip[2], val_rep = 5,
#' doy = 2, percentile_df = percentile_df)
#' @export
helper_check_frequent_val <-  function(val, val_rep,doy, percentile_df){

  if(val_rep >= 9){
    col_check <- "30%"
  } else if(val_rep >= 8){
    col_check <- "50%"
  } else if(val_rep >= 7){
    col_check <- "70%"
  } else if(val_rep >= 5){
    col_check <- "90%"
  } else{
    stop("Repitions not higher than the needed minimum of repitions (=5)")
  }
  
  #check if value is available in percentile df, if it is NA, return FALSE
  if(is.na(percentile_df[doy,col_check]) == T){
    return(FALSE)
  } else{
    
    #return test result, if val is greater or equal to percentile
    return(val >= as.numeric(percentile_df[doy,col_check])) 
  }
}