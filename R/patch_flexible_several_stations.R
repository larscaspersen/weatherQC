#' Patch several weather stations at the same time
#' 
#' This is a wrapper function for patching methods, which only can patch one 
#' station at a time.
#' 
#' The advantage of this function is, that it can be also used for patching
#' methods not supplied by the weatherImpute package. The only demands the 
#' functio does, is that the arguments "weather", "target" and "weather_info"
#' must be used by the user-defined patching function.
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param target character, column name in weather, on which the method should be applied
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param method character, exact name of the patching function, needs to be 
#' present in the workspace
#' @param additional_input list with all the other arguments required by the
#' patching function. elements of list need to have same name as arguments
#' of the patching function
#' @param method_patches_everything flag, indicates if the method returns all
#' weather stations or just the target station. This flag is included, so that
#' patch_flexible_several_stations is also compatible with patching methods
#' like patch_mice or patch_amelia, which return all patched weather stations
#' at one function call
#' @return same data.frame as weather but with all NAs imputed for all columns. There
#' can be still cases of NA, if for a certain observation none of the other stations
#' had valid observations
#' @examples #think of example here
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
patch_flexible_several_stations <- function(weather, target, weather_info, 
                                            method = 'patch_mean', additional_input = list(n_donors = 5),
                                            method_patches_everything = F){
  
  
  
  
  #if the method automatically patches the whole dataframe, then there is no need to call it several times
  #this is true for climatol, amelia and mice
  #in these cases set target to the first target (because it shouldn't matter and return data for every station)
  if(method_patches_everything){
    
    #standard arguments for weather patching function call
    inputs <- list(weather = weather, target = target[1], weather_info = weather_info)
    
    #prepare additional argents that need to be added to the function call
    #only add additional arguments if there are any
    if(length(additional_input) == 1){
      
      #check if additional input is na, in that case do not add it to inputs
      if(!is.na(additional_input)){
        inputs <- c(inputs, additional_input)
      }
    } else{
      #case that there are more than one arguments in additional inputs, then add it no matter what
      inputs <- c(inputs, additional_input)
    }
    
    
    #call function with arguments
    weather_patched <- do.call(method, inputs)
    
    #only take weather stations specified in 'target' so that it is also compatible with function calls where 
    #the user is only interested in some of the stations
    weather_patched <- as.data.frame(weather_patched)[,target]
    
    # #add Date
    # weather_patched <- cbind.data.frame(weather$Date, weather_patched)
    # 
    # #adjust colnames
    # colnames(weather_patched) <- c('Date', target)
    
    
    
  } else {
    #case that the method only returns the fixed data of the target weather station
    
    #iterate over target stations
    patched_stations <- lapply(target, function(x){
      
      #standard arguments for weather patching function call
      inputs <- list(weather = weather, target = x, weather_info = weather_info)
      
      #prepare additional argents that need to be added to the function call
      #only add additional arguments if there are any
      if(length(additional_input) == 1){
        
        #check if additional input is na, in that case do not add it to inputs
        if(!is.na(additional_input)){
          inputs <- c(inputs, additional_input)
        }
      } else{
        #case that there are more than one arguments in additional inputs, then add it no matter what
        inputs <- c(inputs, additional_input)
      }
      
    
      #call function on the inputs, IMPORTANT method must be the same name as the function in the environment
      return(do.call(method, inputs))
    })
    
    #bind list of data frames to one by column
    weather_patched <- do.call(cbind, patched_stations)
  }
  
  
  #add Date
  weather_patched <- cbind.data.frame(weather$Date, weather_patched)
  
  #adjust colnames
  colnames(weather_patched) <- c('Date', target)
  
  return(weather_patched)
  
}