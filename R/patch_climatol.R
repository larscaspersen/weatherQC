#' Impute gaps in daily weather data using climatol package
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with iterated normal-ration method of the climatol package.
#' 
#' For more details of the function please refer to \code{\link[climatol]{homogen}}
#' 
#' Climatol requires the weather data via saved objects instead of the workspace
#' in R and also returns the output as such. In the process of running climatol
#' this function at first creats the input files in the working directory, retrieves
#' the climatol from the objects created by climatol and then tries to clean up after
#' climatol.
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param target redundant argument, just needed to be compatible with downstream 
#' functions
#' @return same data.frame as weather but with all NAs imputed for all columns. There
#' can be still cases of NA, if for a certain observation none of the other stations
#' had valid observations
#' @examples 
#' patch_climatol(weather = weather_Tmin, 
#'               weather_info = rbind(target_info, neighbour_info))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
patch_climatol <- function(weather, weather_info, target = NA){
  
  if( !('elevation' %in% colnames(weather_info))){
    #in case the elevation data was not provided in meta data, download it using elevatr package
   
    elev <- elevatr::get_elev_point(location = weather_info[,c('Longitude', 'Latitude')], prj = "EPSG:4326")
    
    weather_info$elevation <- elev$elevation
    
  }
  
  
  if(sum(is.na(weather_info$elevation))>0){
    stop('Elevation in meta data contains at least one NA')
  }
  
  #bring meta data in format climatol requires
  weather_info <- weather_info[,c('Longitude', 'Latitude', 'elevation', 'id', 'Name')]
  
  #make sure weather is a data.frane
  weather <- data.frame(weather)
  
  #min year
  min_year <- min(weather$Year)
  max_year <- max(weather$Year)
  
  #make sure that the time series starts with January and ends with december
  if((weather$Month[1] != 1 & weather$Day[1] != 1)|
     (weather$Month[nrow(weather)] != 12 & weather$Day[nrow(weather)] != 31)){
    stop('Weather time series needs to start with Jan 1st and end at Dec 31st.')
  }
  
  #drop year month day, date from weather
  weather <- subset(weather, select = -c(.data$Year, .data$Month, .data$Day, .data$Date))
  
  #column names in weather need to be the same as in weather_info$id
  if(any(colnames(weather) %in% weather_info$id == F)){
    stop('There is at least one column in "weather" which is not present in weather_info$id')
  }
  
  #bring weather in same order as in weather_info$id
  weather <- weather[,weather_info$id]
  
  #make sure the id and name are not duplicated, otherwise use make_unique
  if(any(duplicated(weather_info$id))){
    #make unique ids, apply to weather_info$id and colnames(weather)
    id <- make.unique(weather_info$id)
    weather_info$id <- id
    colnames(weather) <- id
  }
  
  if(any(duplicated(weather_info$Name))){
    weather_info$Name <- make.unique(weather_info$Name)
  }
  
  #save object in workspace
  utils::write.table(weather_info, paste0('var_',min_year,'-', max_year,'.est'), row.names=FALSE, col.names=FALSE)
  
  #write table of observations
  write(as.matrix(weather), file = paste0('var_',min_year,'-',max_year,'.dat'))
  
  #run patching
  climatol::homogen('var', as.numeric(min_year), as.numeric(max_year), dz.max = 100, snht1 = 0, gp = 0,verb = F)
  
  #load patched data
  load(paste0('var_',min_year,'-',max_year,'.rda'))
  
  #adjust colnames
  dah <- as.data.frame(dah)
  colnames(dah) <- weather_info$id
  
  #remove files created by climatol
  pattern <- paste0('var_',min_year,'-',max_year)
  fnames <- list.files(pattern = pattern)
  unlink(x = fnames)
  
  #returns patched data of every station
  return(dah)
  
}