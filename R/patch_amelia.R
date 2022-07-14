#' Impute gaps in daily weather data using multiple imputation of Amelia package
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with multiple imputation method of the Amelia package.
#' 
#' For more details of the function please refer to \code{\link{Amelia::amelia}}
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param target character, column name in weather, on which the method should be applied
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param rain_data logical, in case precipitation data should be patched the 
#' mice package allows to at first decide if precipitation occures and in a
#' second step to patch precipitation amount only in cases where the patching
#' algorithm expects precipitation to happen.
#' @param prcp_threshold numerical, setting the threshold until which
#' precipitation amount the algorithm treats observations are real precipitation
#' events. Values below are treated as zeros
#' @param n_impute number of multiple imputations, default is 5
#' @param parallel logical, if true the paralleled version Amelia::amelia is used
#' @return same data.frame as weather but with all NAs imputed for all columns. There
#' can be still cases of NA, if for a certain observation none of the other stations
#' had valid observations
#' @examples #think of example here
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
patch_amelia <- function(weather, target, weather_info, rain_data = F, 
                         prcp_threshold = 1, n.impute = 5,
                         parallel = NA){
  
  #bring date to date-format
  weather$Date <- as.Date(weather$Date)
  
  
  
  #only transform data if it is rain data
  if(rain_data){
    #bring in long format
    weather.long <- reshape2::melt(weather, id.vars = c('Date', 'Year', 'Month', 'Day'))
    
    #transform by log(x+1)
    weather.long$value <- log(weather.long$value + 1)
    
    #bring back to wide format
    weather <- reshape2::dcast(weather.long, formula = Date + Year + Month + Day ~ variable, value.var = 'value' )
  }
  
  #run amelia patching on weather data
  weather.imp <- Amelia::amelia(weather, ts = 'Date', m = n.impute,
                                idvars = c('Year', 'Month', 'Day'),parallel = parallel)
  
  #extract the imputed data from the object
  weather.imp <- weather.imp$imputations
  
  if(rain_data){
    #reverse transformation
    weather.imp <- lapply(weather.imp, function(x) exp(x[,weather_info$id])-1)
  }
  
  
  #bind imputation round by column for each station
  weather.sum <- lapply(weather_info$id, function(x){
    
    #take data of a certain weather station
    extracted_station <- lapply(weather.imp, function(y) y[,x])
    
    #bind it by columns for the different imputation rounds
    extracted_station <-  as.data.frame(do.call(cbind, extracted_station))
    
    #calculate row mean
    return(rowMeans(extracted_station))
    
  })
  
  #bind summarised data of the stations
  weather.sum <- as.data.frame(do.call(cbind, weather.sum))
  
  
  
  #in case of rain data, impute the occurence of rain and then correct the rain amount with occurence data
  if(rain_data){
    
    #drop unwatned columns
    ignore <- c('Date', 'Year', 'Month', 'Day')
    
    #create dummy dataframe with occurence
    dummy_df <- as.data.frame(dplyr::select(weather, -ignore) > prcp_threshold)
    
    #change columns to factors
    index <- 1:ncol(dummy_df)
    dummy_df[ , index] <- lapply(dummy_df[,index], as.factor)
    
    #add date as time series indicator
    dummy_df$Date <- weather$Date 
    
    #impute occurence
    occ.imp <- Amelia::amelia(dummy_df, m = n.impute, ts = 'Date', 
                              noms = weather_info$id, parallel =  parallel)
    
    #function to get mode
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    
    #bind imputation round by column for each station
    occ.sum <- lapply(weather_info$id, function(x){
      
      #take data of a certain weather station
      extracted_station <- lapply(occ.imp$imputations, function(y) y[,x])
      
      #bind it by columns for the different imputation rounds
      extracted_station <-  as.data.frame(do.call(cbind, extracted_station))
      #this step changes the logical to numerical with 1 and 2, because do.call binds to matrix, I guess...
      #1 == no rain, 2 == rain
      
      #calculate row mode of occurence
      return(apply(as.data.frame(extracted_station), MARGIN = 1, FUN = Mode))
      
    })
    
    #bind summarised data of the stations
    occ.sum <- as.data.frame(do.call(cbind, occ.sum))
    
    #change levels of occurence to a different code: 1 is rain occurence, 0 is no occurence
    occ.sum <- occ.sum - 1
    
    #--> multiply the data frames with each other, in cases there is no occurence the value is automatically set to zero
    weather.sum <- weather.sum * occ.sum
    
    #set prcp amount to zero in cases of amounts lower than threshold
    weather.sum[weather.sum < prcp_threshold] <- 0
    
  }
  
  #adjust column names
  colnames(weather.sum) <- weather_info$id
  
  #return imputed object
  return(weather.sum)
}