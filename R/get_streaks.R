#' Identify unusual repetitions in weather data
#' 
#' Function to detect unusual amount of repetitions in daily temperature and precipitation
#' data.
#' 
#' Before the function searches for streaks, missing data are dropped and in
#' case of precipitation observations of no precipitation as well. If in 20 (that is 
#' the default threshold) subsequent days of the remaining data the exact same
#' value of the variable of interest is noted, then the whole streak is marked by
#' a flag.
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 3 "Basic integrity tests".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable column name in \code{weather} for which the test is performed. Should
#' be either Tmin or Tmax. data.frames in \code{aux_list} need to have the same
#' name
#' @param rep_threshold test limit, if streaks this length or greater are found, 
#' then test returns positive flag
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious. Data which was dropped in the process of the analysis receives
#' automatically a FALSE
#' @examples get_streaks(weather = weather, variable = "Tmin")
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
get_streaks <- function(weather, variable, rep_threshold = 20){
  
  #if not present, add date to weather
  if(!('Date' %in% colnames(weather))){
    #add Date
    weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                            format = '%Y-%m-%d')
  }
  
  #only take variable of interest
  x <- weather[,c('Date', variable)]
  names(x) <- c('Date', 'trials')
  #remove missing values
  x <- x[is.na(x$trials) == F,]
  
  #in case of precipitation: also remove zeros
  if(variable == 'Precip'){
    x <- x[x$trials != 0,]
  }
  
  x <- x %>%  mutate(lagged=lag(trials)) %>% #note: that's dplyr::lag, not stats::lag
    mutate(start=(trials != lagged))
  x[1, "start"] <- TRUE
  x <- x %>% mutate(streak_id=cumsum(start))
  x <- x %>% group_by(streak_id) %>% mutate(streak=row_number()) %>%
    ungroup()
  
  #filter days which exceed the streak id
  sub <- x %>%
    filter(streak >= rep_threshold)
  
  #get all days belonging to the streaks, mark those days as suspicious
  sus_days <- x[x$streak_id %in% sub$streak_id,'Date']
  
  return(weather$Date %in% pull(sus_days))
}