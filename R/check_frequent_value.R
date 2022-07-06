#' Check for duplicated values in precipitation
#' 
#' The function screens for duplicated precipitation values within a sliding
#' 10 day window.
#' 
#' This is the corresponding version of \code{\link{get_streaks}} for precipitation.
#' The function drops all NA and zero-precipitation observation from the test and
#' uses the remaining data. This means a sequence like c(10, 0, 5, NA, 0, 5) will 
#' be evaluates as c(10, 5, 5) in the test. Within a sliding 10-day window frequent
#' values are screened for. If the frequently duplicated precipitation value is
#' repeated 5 or more times within that time-window it will be compared to a 
#' climatological precipitation percentile for the first day of the time window.
#' The more repetitions found, the lower the compared precipitation percentile will be.
#' If the frequently repeated value exceeds the percentile, each of the repeated
#' precipitation values will be flagged.
#' 
#' For 5 or 6 repetitions the 90\% percentile will be used, for 7 the 70\% percentile,
#' for 8 repetitions the 50\% percentile and for 9 or 10 repetitions the 30\% percentile.
#' 
#' Climatological precipitation percentiles are calculated for each day of the year,
#' using a 29-day window centered at the day of interest. All precipitation observation
#' throughout the recorded years lying within that time window will be included in
#' the percentile calculation. Missing values and zero-precipitation observations
#' will be discarded. By default there need to be at least 20 non-zero observation
#' for the percentile calculation, otherwise there will be NA instead of percentiles.
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 3 "Basic integrity tests".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param percentile_df data.frame containing the climatological precipitation
#' percentiles for the corresponding day of the year
#' @param min_non_zero_days minimum amount of non-zero precipitation observation need 
#' to be present for ecdf calculation, otherwise NA returned
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious. Data which was dropped in the process of the analysis receives
#' automatically a FALSE
#' @examples 
#' percentile_df <- get_each_day_precipitation_percentile(weather = weather)
#' check_frequent_value(weather = weather, percentile_df = percentile_df)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
check_frequent_value <- function(weather, percentile_df, min_non_zero_days = 20){
  
  #incase there is no doy in weather, add it
  if(!'doy' %in% names(weather)){
    #in case no date in weather add it too
    if(!'Date' %in% names(weather)){
      weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                              format = '%Y-%m-%d')
    }
    weather$doy <- lubridate::yday(weather$Date)
  }
  
  #drop na values, and 0
  x <- weather %>%
    .[is.na(.$Precip) == FALSE,] %>%
    filter(Precip > 0)
  
  #flag 
  frequent_value_flag <- rep(F, nrow(x))
  
  for(i in 1:(nrow(x)-9)){
    
    #check for the day if there is a minimum of repitions
    if(any(table(x[i:(i+9),'Precip'])>=5)){
      
      if(sum(table(x[i:(i+9),'Precip'])>=5) == 1){
        #extract the value of repitions and check if the repeated value exceeds the limits of the test
        val_rep <- table(x[i:(i+9),'Precip'])[table(x[i:(i+9),'Precip'])>=5]
        
        #look up if repeated value exceeds the threshold in percentile df
        if(helper_check_frequent_val(val = as.numeric(names(val_rep)), val_rep = val_rep, 
                                     doy = weather$doy[i], percentile_df = percentile_df)){
          
          #if check is positive, then flag is changed
          #identify which positions need to be flagged
          frequent_value_flag[i+which(x[i:(i+9),'Precip'] == as.numeric(names(val_rep))) -1] <- TRUE
          
        }
        
      } else{
        #case that we have two times repitions of 5
        sus_val_present <- as.numeric(names(table(x[i:(i+9),'Precip']))) >= as.numeric(percentile_df[weather$doy[i],"90%"])
        
        #in case the check returned NA, change it to FALSE instead
        sus_val_present[is.na(sus_val_present)] <- FALSE
        
        #at least one exceeds the test
        if(any(sus_val_present)){
          #identify which ones exceed the test
          sus_val <- as.numeric(names(table(x[i:(i+9),'Precip'])[sus_val_present]))
          #identify position of values in x
          sus_position <- i+which(x[i:(i+9),'Precip'] %in% sus_val)-1
          #change flag accordingly
          frequent_value_flag[sus_position] <- TRUE
        }
      }
      
    }
    
  }
  
  #need to bring frequent value flag to same size as weather again
  
  #bind flag to x
  x$flag <- frequent_value_flag
  
  #merge x and weather
  weather <- merge(weather, x, by = colnames(weather), all.x = T) %>%
    arrange(Date)
  
  #change NAs in Flag to FALSE
  weather$flag[is.na(weather$flag)] <- FALSE
  
  #return flag
  return(weather$flag)
  
}