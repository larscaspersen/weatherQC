#' Automated comparison and evaluation of several weather patching methods
#' 
#' This function allows the evaluation and comparison of several patching methods for the same
#' weather data.
#' 
#' Evaluation is done by identifying the longest continuous NA-free period in the
#' target weather data and inserting p_missing amount of NA for desired weather station
#' in \code{weather}. Targeted weather stations are also used as auxiliary weather stations
#' along with non-targeted weather stations in \code{weather}.
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param target character, column name in weather, on which the method should be applied
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param patch_methods, character vector with the exact name of the patching 
#' functions which should evaluated. Needs to be the same length as \code{additional_args}
#' @param p_missing numerical, indicates the amount of missing data inserted to
#' the evaluation period, relative to the evaluation period length. A value of 1 
#' would remove each observation in the evaluation period, a value of 0 would
#' lead to no new gaps at all
#' @param additional_args by default NULL, if additional arguments of the patching
#' functions should be specified, then it needs to happen in form of a list
#' with all the other arguments required by the
#' patching function. elements of list need to have same name as arguments
#' of the patching function
#' @param method_patches_everything flag, indicates if the method returns all
#' weather stations or just the target station. This flag is included, so that
#' patch_flexible_several_stations is also compatible with patching methods
#' like patch_mice or patch_amelia, which return all patched weather stations
#' at one function call
#' @param period by default NULL, in that case the function searches for the longest
#' continuous NQA-free period for each weather station. If user-specified
#' period is wished for, it needs to be a numerical vector of length two, 
#' row numbers of evaluation period
#' @param mute boolean, by default set TRUE. If set FALSE, then function 
#' keeps user updated on which patching function it is currently working on
#' @param return_data character, indicates what the function returns after successfull
#' evaluation. Valid options are "only_new_imputed", which returns only the original and
#' newly imputed observations per weather station, "evaluation_period" which returns
#' the whole evaluation period including non-imputed observations and "everything"
#' which returns a data frame with the same amount as \code{weather}
#' @return see the description in \code{return_data}
#' @examples 
#' get_eval_one_station(weather = weather_Tmin,
#' weather_info = rbind(target_info, neighbour_info),
#' target = 'cimis_2', 
#' patch_methods = c('patch_idw','patch_normal_ratio'), 
#' method_patches_everything = c(TRUE, FALSE))
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
get_eval_one_station <- function(weather, weather_info, target, 
                                 patch_methods, 
                                 p_missing = 0.3, 
                                 additional_args = NULL, 
                                 method_patches_everything = FALSE,
                                 period = NULL, 
                                 return_data = 'only_new_imputed',
                                 mute = TRUE){
  
  #method patches everything needs to be of the same length as method
  
  
  #check if weather data contains columns of Date, Year, Month, Day
  if(!("Year" %in% colnames(weather) & "Month" %in% 
        colnames(weather) & "Day" %in% colnames(weather) & "Date" %in% colnames(weather))){
    stop("Required input column 'Year', 'Month', 'Day' and/or 'Date' is missing.")
    }
  
  #make sure that weather is a data.frame
  weather <- data.frame(weather)
  
  #check if columns Longitude, Latitude and id are present in weather_info
  if(any(c("id", "Longitude", "Latitude") %in% colnames(weather_info) == FALSE)){
    stop("columns c('id', 'Longitude', 'Latitude') need to be present in weather_info.
         Maybe also check your spelling in the column names and if capitalized.")
  }
  
  
  ######
  #check period data
  ######
  

  #in case no period provided, in which holes should be punched

  if(is.null(period)){
    
    #apply the search of NA free period to all target stations
    #as a result have a list of the periods, in the order of the stations
    period_list <- lapply(target, function(x){
      
      #check if weather station has missing data
      if(sum(is.na(weather[x])) == 0){
        
        #in that case the period covers the whole dataframe
        return(c(1, nrow(weather)))
      } else {
        
        #look for the longest period without na
        longest_coverage <- stats::na.contiguous(weather[,x])
        
        #get the row number of start and end of longest continous period
        period <- stats::tsp(longest_coverage)
        
        return(period)
      }
    })
    
    
    
  } else{
    #check if provided period is of right format:
    #can be supplied as list, but then it should have the same amount of elements as in target
    if(is.list(period)){
      if(length(period) != length(target)){
        stop('If period is supplied in form of list, then it should have the same length as target')
        
        #check the first element of period, if stored as list, should be of length two and should be numeric
      } else if(length(period[[1]]) != 2 | is.numeric(period[[1]]) == FALSE){
        stop('Elements of period, when suppplied as a list, should be of length = 2 and should be numeric, in form of c(rownumber start, rownumber stop).')
        
        #case that everything is in right format
      } else if(length(period) == length(target) & length(period[[1]] == 2)){
        
        #assign period to period list
        period_list <- period
      }
      
      
      #if period supplied not in form of list, then it should be of length == 2 and numeric, as it is then applied on every station
    } else if(is.numeric(period)){
      if(length(period) != 2){
        stop('If period supplied in numeric form, then it needs to be in length of 2. Then it is applied on every target station')
      } else if(length(period) == 2){
        
        #if supplied in numeric form, bring it also to list form to be consistent
        period_list <- rep(list(period), length(target))
      }
      #if neither in form of list or in form of numeric, then invalid form 
    } else{
      stop('Period supplied in wrong format. Needs to be either in form of list (with same length as target, each element numeric of length two) or numeric (then length == 2, applied on every station.')
    }
    
  }
  
  
  
  #####
  #punch wholes in the different parts of the stations
  #####
  
  #period-list should be in the same order as target
  #--> add name to the period list as third element
  period_list <-  mapply(function(x,y){c(x[1:2],y)}, period_list, target, SIMPLIFY = FALSE)
  
  # #prepare data, combine target with target name in a list
  # prep_data <- mapply(function(x,y){
  #   list(station = x, period = y)
  # }, target, period_list, SIMPLIFY = T)
  
  
  mod_weather <- lapply(period_list, function(x){
    
    #calculate how many Days should be removed from the station to achieve the desired missingness
    n_new_holes <- floor((as.numeric(x[2]) - as.numeric(x[1]) + 1) * p_missing)
    
    #sample from the rows of the target weather station, sampled rows get deleted
    row_holes <- sample(x = as.numeric(x[1]):as.numeric(x[2]), size = n_new_holes, replace = FALSE)
    
    prep_df <- data.frame(weather[,x[3]], new_na = FALSE)
    #add na at position of row holes
    
    prep_df[row_holes,1] <- NA
    prep_df[row_holes,2] <- TRUE
    
    colnames(prep_df) <- c(x[3], paste0(x[3], '_new_na'))
    
    return(prep_df)
  })
  
  # 
  # 
  # #for each data, punch random holes in the before identified period
  # prep_data <- apply(prep_data,MARGIN = 2, function(x){
  #   
  #   #calculate how many Days should be removed from the station to achieve the desired missingness
  #   n_new_holes <- floor((x[['period']][2] - x[['period']][1] + 1) * p_missing)
  #   
  #   #make a subset for the evaluation, which is the longest period of observation for the 
  #   #target station
  #   eval_sub <- weather[x[['period']][1]:x[['period']][2], x[['station']]]
  #   
  #   #sample from the rows of the target weather station, sampled rows get deleted
  #   row_holes <- sample(x = x[['period']][1]:x[['period']][2], size = n_new_holes, replace = FALSE)
  #   
  #   #punch holes in weather data
  #   weather[row_holes, x[['station']]] <- NA
  #   
  #   #make new data frame with new data of target station with holes, also add column which indicates where new NAs were created
  #   prepared_df <- data.frame(weather[, x[['station']]], new_na = FALSE)
  #   colnames(prepared_df) <- c(x[['station']], paste0(x[['station']], '_new_na'))
  #   prepared_df[row_holes,2] <- TRUE
  #   
  #   return(prepared_df)
  # 
  # })
  # 
  # #get rid of annoying names in list
  # prep_data <- unname(prep_data)
  
  #bind weather data
  mod_weather <- do.call(cbind, mod_weather)
  
  
  # #extract weather data (because there are also columns indicating if the NA is new or if it has been there before)
  # weather_eval <- prep_data[,target]
  # weather_eval <- cbind(weather[,c('Date', 'Year', 'Month', 'Day')], weather_eval)
  #weather eval should have the same format as weather, same columns
  #--> replace the columns of weather by targt
  # weather_eval <- weather
  
  #only take weather modified weather station data
  weather_eval <- cbind(weather[,c('Year', 'Month', 'Day', 'Date')], mod_weather[, target])
  
  #make sure that names are still correct
  names(weather_eval) <- c("Year", "Month", "Day", 'Date', target)
  
  #add non-target weather stations to weather-eval aswell
  weather_eval <- cbind(weather_eval,
        weather[,colnames(weather) %in% c("Year", "Month", "Day", "Date", target) == FALSE])
  
  
  
  #dataframe indicating if new hole created per weather station targeted
  holes_df <- dplyr::select(mod_weather, -dplyr::all_of(target))
  
  #add date column
  holes_df <- cbind(Year = weather$Year, Month = weather$Month, Day = weather$Day,
                    Date = weather$Date, holes_df)
  
  
  
  
  #####
  #run patching methods
  #####
  
  #check if additional args is null
  if(is.null(additional_args)){
    prep_data <- mapply(function(x,y){
      list(method = x, additional_args = NULL, method_patches_everything = y)
    }, patch_methods, method_patches_everything, SIMPLIFY = FALSE)
  } else{
    prep_data <- mapply(function(x,y,z){
      list(method = x, additional_args = y, method_patches_everything = z)
    }, patch_methods, additional_args, method_patches_everything, SIMPLIFY = FALSE)
  }

  
  #run patching function on evaluation data
  patched <- lapply(prep_data, function(x){
    
    #give info which method is now used
    if(mute == FALSE){
      cat(paste0('Start patching method: ', x[['method']]))
    }
    
    
    #carryout patch function
    patched <- invisible(patch_flexible_several_stations(weather = weather_eval, 
                                                         target = target, 
                                                         weather_info = weather_info, 
                                                         method = x[['method']],
                                                         additional_input = x[['additional_args']], 
                                                         method_patches_everything = x[['method_patches_everything']]))
    
    #bring to long format
    patched <- reshape2::melt(patched, measure.vars = target, 
                              variable.name = 'station', value.name = x[['method']])
    
    #return only value column of long format patched
    return(patched)
  })
  
  patched <- unname(patched)
  
  #output is list with station and value per patch method
  #--> bind them by column
  patched <- do.call(cbind.data.frame, patched)
  
  #duplicated columns of date and station; at first adjust column names
  names(patched) <- make.unique(names(patched), sep = '_')
  
  #adjust names of patch_methods if duplicated
  if(sum(duplicated(patch_methods)) > 0){
    patch_methods <- make.unique(patch_methods, sep = '_')
    warning('At least two patching methods have same name, column names were made unique by appending _1 on the second duplicated patching name and so on')
  } 
  
  #only take Year, Month, Day, Date and patch_methods
  patched <- patched[, c('Date', 'station', patch_methods)]
  
  
  #add info of weather station because patched is already in long format
  holes_df <- reshape2::melt(holes_df, variable.name = 'station', 
                             id.vars = c('Year', 'Month', 'Day', 'Date'), value.name = 'new_na')
  #strip the _new_na from the variable var
  holes_df$station <- gsub(pattern = '_new_na', replacement = '', x = holes_df$station)
  
  
  #merge the two dataframes 
  patched <- dplyr::left_join(x = patched, y = holes_df, by = c('Date', 'station'))
  
  
  #add info on evaluation period
  period_list <- lapply(period_list, function(x){
    period_vec <- rep(FALSE, nrow(weather_eval))
    period_vec[as.numeric(x[1]):as.numeric(x[2])] <- TRUE
    
    return(period_vec)
  })
  
  #add info which part belongs to evaluation period
  patched$eval_period <- do.call(c, period_list)
  
  #add original weather to patched
  weather_long <- reshape2::melt(weather, id.vars = c('Year', 'Month', 'Day', 'Date'),
                                 value.name = 'original', variable.name = 'station')
  
  patched <- dplyr::left_join(x = patched, y = weather_long, 
                              by = c('Year', 'Month', 'Day', 'Date', 'station'))
  
  #now either return everything, everyhtin where eval_period is true or everything where new_na is true
  
  
  
  #three options: 1) return only the imputed data 
  #               2) return the whole evaluation period per station
  #               3) return the complete patched weather stations
  
  
  #option 1: return only newly imputed data
  if(return_data == 'only_new_imputed'){
    return(patched[patched$new_na, c('Date', 'Year', 'Month', 'Day', 'station', 'original', patch_methods)])
    
    #option 2: return the whole period, which was used to randomly insert NAs  
  } else if(return_data == 'evaluation_period' ){
    return(patched[patched$eval_period, c('Date', 'Year', 'Month', 'Day', 'station', 'original', patch_methods, 'new_na')])
    
    #option 3: return everything
  } else if(return_data == 'everything'){
    
    return(patched[, c('Date', 'Year', 'Month', 'Day', 'station', 'original', patch_methods, 'new_na', 'eval_period')])
  }
  
}
