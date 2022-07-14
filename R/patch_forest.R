#' Impute gaps in daily weather data using random forest method
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with random forest method of the missforest package.
#' 
#' For more details of the function please refer to \code{\link{missforest::missforest}}
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
#' @param n_donors amount of auxiliary observations used to fill the gap, by default 5
#' @param donor_criterion character, method used to select donors, if there they are
#' in excees available. Valid options are "closest" and "correlation" 
#' @param max.iter maximum amount of iterations of the multiple imputation algorithm,
#' default is 5
#' @return vector, containing the imputed weather observations of target station.It is
#' still possible, that cases of NA remain for days none or not enough neighboring stations
#' had observations available
#' @examples #think of example here
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
patch_forest <- function(weather, target, weather_info, rain_data = F, 
                         prcp_threshold = 1, 
                         n_donors = 5, donor_criterion = 'closest', max.iter = 5){
  
  #columns to be ignored in the following
  ignore <- c('Year', 'Month', 'Day', 'Date')
  
  #drop columns which should not be imputed
  weather <- dplyr::select(weather, -ignore)
  
  if(donor_criterion == 'closest'){
    
    #extract lon and lat of target station
    target_lon <- weather_info[weather_info$id == target, 'longitude']
    target_lat <- weather_info[weather_info$id == target, 'latitude']
    
    #calculate distances between stations, choose the n closesest ones
    weather_info$distance <- round(sp::spDistsN1(as.matrix(weather_info[, c("longitude", "latitude")]),
                                              c(target_lon, target_lat), longlat = TRUE), 2)
    
    #sort to by increasing distance
    weather_info <- weather_info[order(weather_info$distance),]
    
  } else if(donor_criterion == 'correlation'){
    
    #calculate correlations, take only the ones regarding the target station
    weather.cor <- cor(weather, use = 'pairwise.complete.obs' )[,target]
    
    #create data frame of correlations, use names as id
    weather.cor <- data.frame(cor = weather.cor, id = names(weather.cor))
    
    #match with meta data
    weather_info <- merge.data.frame(weather_info, weather.cor, by = 'id')
    
    #sort metadata by decreasing correlations
    weather_info <-  weather_info[order(weather_info$cor, decreasing = T),]
  }
  
  #take donors from ordered weather_info 
  
  if(n_donors >= ncol(weather)){
    donors <- weather_info$id[2:nrow(weather_info)]
  } else {
    #case if we have enough donors
    donors <- weather_info$id[2:(n_donors + 1)]
  }
  
  #take data of target and donors
  weather <- dplyr::select(weather, all_of(c(target, donors)))
  
  
  #impute variable of interest
  weather.imp <- missForest::missForest(weather, maxiter = max.iter)
  
  #extract target column
  weather.imp <- weather.imp$ximp[,target]
  
  
  if(rain_data){
    
    #make dummy dataframe of occurence
    dummy_df <- as.data.frame(weather > prcp_threshold)
    
    #change columns to factors
    index <- 1:ncol(dummy_df)
    dummy_df[ , index] <- lapply(dummy_df[,index], as.factor)
    
    #impute occurence
    occ.imp <- missForest::missForest(dummy_df, maxiter = max.iter)
    occ.imp <-  occ.imp$ximp[,target]
    
    #correct imputed rain amounts by occurence
    weather.imp[occ.imp == FALSE] <- 0
    
    #prevent values below zero, set values below threshold to zero
    weather.imp[weather.imp < prcp_threshold] <- 0
  }
  
  return(weather.imp)
}