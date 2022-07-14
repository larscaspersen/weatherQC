#' Impute gaps in daily weather data using climatol package
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with iterated normal-ration method of the climatol package.
#' 
#' For more details of the function please refer to \code{\link{climatol::homogen}}
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
#' @param target character, column name in weather, on which the method should be applied
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @return same data.frame as weather but with all NAs imputed for all columns. There
#' can be still cases of NA, if for a certain observation none of the other stations
#' had valid observations
#' @examples #think of example here
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
patch_climatol <- function(weather, target, weather_info){
  
  if( !('elevation' %in% colnames(weather_info))){
    #in case the elevation data was not provided in meta data, download it using elevatr package
    library(rgdal)
    elev <- elevatr::get_elev_point(location = weather_info[,c('longitude', 'latitude')], prj = "EPSG:4326")
    
    weather_info$elevation <- elev$elevation
    
  }
  
  if(sum(is.na(weather_info$elevation))>0){
    stop('Elevation in meta data contains at least one NA')
  }
  
  #bring meta data in format climatol requires
  weather_info <- weather_info[,c('longitude', 'latitude', 'elevation', 'id', 'name')]
  
  #min year
  min_year <- min(weather$Year)
  max_year <- max(weather$Year)
  
  #save object in workspace
  write.table(weather_info, paste0('var_',min_year,'-', max_year,'.est'), row.names=FALSE, col.names=FALSE)
  
  
  #make sure weather data columns are of the same order as in weather_info, also drops not needed columns
  weather <- weather[, weather_info$id]
  
  #write table of observations
  write(as.matrix(weather), file = paste0('var_',min_year,'-',max_year,'.dat'))
  
  #run patching
  climatol::homogen('var', as.numeric(min_year), as.numeric(max_year), dz.max = 100, snht1 = 0, gp = 0,verb = F)
  
  
  ####temp
  
  #load patched data
  #load(paste0('var_',min_year,'-',max_year,'.rda'))
  
  ####temp
  
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