#' Patch daily weather data with the adjusted normal-ratio method
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with the adjusted normal-ratio method.
#' 
#' Fleshout the description. Add relevant papers
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
patch_normal_ratio <- function(weather, target, weather_info, n_donors = 5){

#extract lon and lat of target station
target_lon <- weather_info[weather_info$id == target, 'longitude']
target_lat <- weather_info[weather_info$id == target, 'latitude']

#calculate distances between stations, choose the n closesest ones
weather_info$distance <- round(sp::spDistsN1(as.matrix(weather_info[, c("longitude", "latitude")]),
                                          c(target_lon, target_lat), longlat = TRUE), 2)

#sort to by increasing distance
weather_info <- weather_info[order(weather_info$distance),]


#get correlation coefficient 
corr_weather <- weather %>%
  dplyr::select(-c('Date', 'Year', 'Month', 'Day')) %>%
  cor(use = "pairwise.complete.obs")

#get cases in which we have observation
x <- weather %>%
  dplyr::select(-c('Date', 'Year', 'Month', 'Day')) %>%
  is.na()
x <- !x

#get sum of pairwise observations
n_pairwise_obs <- t(x) %*% x


#identify rows of missing data in target
row_na <- which(is.na(weather[,target]))

#for each missing Day in target, return names of weather stations with data
stat_obs <- lapply(row_na, FUN = function(x){
  return(list(row = x, sation_id = colnames(weather)[!is.na(weather[x,])]))
})




#define function to calculate NR for stations with reading at that time 
nr_of_closest_stations <- function(weather, stations_observed, weather_info, 
                                   target,
                                   corr_weather = NA, 
                                   n_pairwise_obs = NA,
                                   n_donors = 5, weight_type = 'ordinary'){
  
  #extract the information of target row and the closest stations by 
  #increasing distance with readings for the target row
  target_row <- stations_observed$row
  closest_station <- stations_observed$sation_id
  
  #select the closest stations
  if(sum(weather_info$id %in% closest_station) == 0){
    #in case no station has a reading at that day, NA remains and a warning will be sent
    impute <- NA
    warning(paste0('For weather station ', target, ' in  row ', target_row, 'there was no neighbour with a reading. Gap remains unfilled'))
    
    #in case of at least one neighbour with a suitable reading, continue with the weightening
  } else {
    if(sum(weather_info$id %in% closest_station) < n_donors){
      
      closest_n <- weather_info$id[which(weather_info$id %in% closest_station)]
    } else {
      #take the n stations with observations, which are the closest
      closest_n <- weather_info$id[which(weather_info$id %in% closest_station)[1:n_donors]]
    }
    
    #get weights
    weights <- lapply(closest_n, function(x){
      r_squared <- corr_weather[target, x]^2
      weight <- r_squared*((n_pairwise_obs[target, x] - 2)/(1 - r_squared))
      return(weight)
    })
    
    #make weights to vector
    weights <- unlist(weights)
    
    impute <- sum(weather[target_row, closest_n] * weights) / sum(weights)
    
    
    
  } #end of case where there are enough neighbours
  
  return(impute)
}


#calculate for each row of na, the normal ratio of the closest stations
impute <- lapply(stat_obs, FUN = function(x){
  nr_of_closest_stations(weather = weather, stations_observed = x,
                         corr_weather = corr_weather, n_pairwise_obs = n_pairwise_obs,
                         weather_info = weather_info, n_donors = n_donors, weight_type = weight_type,
                         target = target)
})


weather[row_na, target] <- unlist(impute)

return(weather[,target])
}