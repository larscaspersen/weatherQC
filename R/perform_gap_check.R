#' Check for large gaps in ordered weather data
#' 
#' Takes daily temperature or precipitation data and looks for unusual large gaps in 
#' ordered data.
#' 
#' The function splits the weather data by month and orders it. In case of temperature
#' it starts from the median towards both tails of the distribution and searches
#' for gaps larger than temp_gap_threshold. If such a gap is found, then all the data
#' coming after the gap towards the end of the tail are flagged. In case of precipitation
#' only the upper tail is investigated and the start is the first non-zero precipitation
#' observation. 
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 4 "Outlier checks".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable 
#' in weather
#' @param temp_gap_threshold testing threshold for temperature data, if gaps are equal
#' or larger, then function flags data. By default 10 degree C
#' @param prec_gap_threshold testing threshold for precipitation data, if gaps are equal
#' or larger, then function flags data. By default 300 mm
#' @return Logical vector of same length as rows in \code{weather}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test.
#' @examples perform_gap_check(weather = weather, variable = "Tmin")
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
perform_gap_check <- function(weather, variable, temp_gap_threshold = 10, 
                              prec_gap_threshold = 300){
  
  #set the threshold for the variable accordingly
  if(variable %in% c('Tmin', 'Tmax', 'Tmean')){
    gap_threshold <- temp_gap_threshold
  } else if(variable == 'Precip'){
    gap_threshold <- prec_gap_threshold
  }
  
  #split data per month
  var_per_month <-  split(weather, weather$Month)
  
  #perform the search for gaps on the monthly split data
  gap_flag <- purrr::map(var_per_month, function(x){
    #drop na values
    x <- x[is.na(x[,variable]) == FALSE,]
    
    #sort decreasing 
    x <- dplyr::arrange(x, x[,variable])
    
    #get monthly flag
    x$gap_flag <-  get_gap_monthly(x = x[[variable]], gap_threshold = gap_threshold)
    
    return(x)
    
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(.data$Date) %>%
    merge.data.frame(weather, .data, by = colnames(weather), all.x = TRUE) %>%
    dplyr::select(gap_flag)
  
  #replace NAs with FALSE values
  gap_flag <- gap_flag[,1] %>%
    tidyr::replace_na(FALSE)
  
  #return gap_flag column
  return(gap_flag)
}