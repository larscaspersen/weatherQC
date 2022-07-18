#' Impute gaps in daily weather data using inverse distance weighting
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with the inverse distance weighting (idw) method. The idw-method 
#' a popular method for example in the missing data imputation of rainfall observations
#' \insertCite{teegavarapu_improved_2005}{weatherQC}. More weight is assigned to 
#' the observations of close-by weather stations. 
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param phi distance weighting factor, the higher the factor, the more penalized
#' far away observations get. By default 2
#' @param target actually this argument is not needed and meaningless, but some higher
#' level function requires it to have this argument
#' @return same data.frame as weather but with all NAs imputed for all columns. There
#' can be still cases of NA, if for a certain observation none of the other stations
#' had valid observations
#' @examples 
#' patch_idw(weather = weather_Tmin, 
#' weather_info = rbind(target_info, neighbour_info))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @references
#' \insertAllCited{}
#' @export
patch_idw <- function(weather, weather_info, phi = 2, target = NULL){
  
  #make sure that weather is data.frame
  weather <- data.frame(weather)
  
  ###temp
  #make points spatial
  p1 <- sp::SpatialPoints(weather_info[, c("Longitude", "Latitude")])
  sp::proj4string(p1) <- "+proj=longlat"
  #calculate distances between all point combinations
  dist <- sp::spDists(p1, p1)
  
  #calculate weights
  weights <- apply(dist, MARGIN = 2, function(x) x^(-phi))
  
  #set weights for own station to zero
  weights[is.infinite(weights)] <- 0
  
  #replace NAs with zeros in weather, so that they dont bother in matrix multiplication
  weather_m <- weather[,weather_info$id]
  weather_full <- weather_m
  weather_full[is.na(weather_full)] <- 0
  
  #do matrix multiplication
  m1 <- (as.matrix(weather_full) %*% weights)
  
  m2 <- apply(m1, 1, function(x){
    x / colSums(weights)
  })
  
  m2 <- t(m2)
  colnames(m2) <- weather_info$id
  
  weather_m[is.na(as.matrix(weather_m))] <- m2[is.na(as.matrix(weather_m))]
  return(weather_m)
  
  ###temp
  
  # #extract lon and lat of target station
  # target_lon <- weather_info[weather_info$id == target, 'longitude']
  # target_lat <- weather_info[weather_info$id == target, 'latitude']
  # 
  # #calculate distances between stations, choose the n closesest ones
  # weather_info$distance <- round(sp::spDistsN1(as.matrix(weather_info[, c("longitude", "latitude")]),
  #                                           c(target_lon, target_lat), longlat = TRUE), 2)
  # 
  # #calculate inverse distance weight
  # weather_info$weight <- weather_info$distance^(-phi)
  # 
  # #put in extra object
  # weight_df <- weather_info[weather_info$id != target,]
  # 
  # #bring weather to extra object, fill every NA with 0 for matrix multiplication
  # weather_full <- weather[weight_df$id]
  # weather_full[is.na(weather_full)] <- 0
  # 
  # #results for each day using idw
  # candidates <- (as.matrix(weather_full) %*% as.matrix(weight_df['weight']))/sum(weight_df$weight)
  # 
  # #select days with missing data in target data and replace it with IDW data
  # weather[which(is.na(weather[,target])), target] <- candidates[which(is.na(weather[,target]))]
  # 
  # return(weather[,target])
  
}