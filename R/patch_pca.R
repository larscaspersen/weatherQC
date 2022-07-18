#' Patch weather data with several types of PCA methods
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with either the "nipals" or "ppca" method. 
#' 
#' This is a simple wrapper function for the \code{\link[pcaMethods]{pca}} and
#' \code{\link[pcaMethods]{completeObs}} functions. 
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param method character, describing which pca method is used. Valid options are
#' "nipals" and "ppca". For more details on the methods please refer to 
#' \code{\link{pcaMethods::pca}}
#' @param nPcs amount of principle components created in the proces of the pca
#' function
#' @param target actually this argument is not needed and meaningless, but some higher
#' level function requires it to have this argument
#' @return vector, containing the imputed weather observations of target station.It is
#' still possible, that cases of NA remain for days none or not enough neighboring stations
#' had observations available
#' @examples 
#' patch_pca(weather = weather_Tmin, 
#' weather_info = rbind(target_info, neighbour_info))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
patch_pca  <- function(weather, weather_info, method = 'nipals', nPcs = 2,
                       target = NULL){
  
  #is this really needed? this should be handled when loading the package
  #and not when using a function
  #if (!require("BiocManager", quietly = TRUE)){
  #  install.packages("BiocManager")
  #  BiocManager::install("pcaMethods")
  #}
  
  
  pc <-  pcaMethods::pca(weather[,weather_info$id],method = method, nPcs = nPcs )
  
  return(pcaMethods::completeObs(pc))
}