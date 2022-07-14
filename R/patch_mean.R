#' Impute gaps with mean observation of neighbouring stations
#' 
#' This function calculates the mean of neighbouring weather stations to fill 
#' observation gap of the target weather station.
#' 
#' For each day the closest weather station with observations are taken and
#' the gap is filled with the mean of the auxiliary observations.
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param target character, column name in weather, on which the method should be applied
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param n_donors amount of auxiliary observations used to fill the gap, by default 5
#' @param max_dist maximum distance in km of a auxiliary weather station, to be considered
#' in the gap imputation
#' @return vector, containing the imputed weather observations of target station.It is
#' still possible, that cases of NA remain for days none or not enough neighboring stations
#' had observations available
#' @examples #still need to think of examples
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export 
patch_mean <- function(weather, target, weather_info, n_donors = 5, max_dist = 150){
  
  #extract lon and lat of target station
  target_lon <- weather_info[weather_info$id == target, 'longitude']
  target_lat <- weather_info[weather_info$id == target, 'latitude']
  
  #calculate distances between stations, choose the n closesest ones
  weather_info$distance <- round(sp::spDistsN1(as.matrix(weather_info[, c("longitude", "latitude")]),
                                            c(target_lon, target_lat), longlat = TRUE), 2)
  
  #sort to by increasing distance
  weather_info <- weather_info[order(weather_info$distance),]
  
  #drop stations which are further than the maxium distance to include
  weather_info <- weather_info[weather_info$distance <= max_dist,]
  
  #if there are less then three stations (excluding target station remaining), then sent a warning and stop
  if(nrow(weather_info) < 4){
    warning('There are less than 3 auxiliary stations within the specified maximum distance. Consider increasing the max_dist parameter or choosing a different patching function')
    return(weather[,target])
  }
  
  #order weather by distance
  weather <- weather[, weather_info$id]
  
  #get mean of closest stations with readings
  means <- apply(as.matrix(weather), MARGIN = 1, function(x){
    
    #amount of non-NA observations for that day of the closest neighbours
    y <- sum(!is.na(x)[2:(2+n_donors-1)])
    
    #amount of extra-columns to include to get n_donros non-NA readings
    i <- 0
    
    #only increase i if there are stations available
    if(length(x) > 2+n_donors-1+i){
      #increase i until we either have enough readings or we run out of columns
      while(y <= n_donors-1){
        i <- i +1
        y <- sum(!is.na(x)[2:(2+n_donors-1+i)])
        if(2+n_donors-1+i >= ncol(weather)) break()
      }
    }
    
    
    #return the mean
    return(mean(x[2:(2+n_donors-1+i)], na.rm = T))
  })
  
  #identify rows of missing data in target
  row_na <- which(is.na(weather[,target]))
  
  #fill gaps with means
  weather[row_na, target] <- means[row_na]
  
  return(weather[,target])
}