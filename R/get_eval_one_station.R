#' Automated comparison and evaluation of several weather patching methods
#' 
#' This function allows the evaluation and comparison of several patching methods for the same
#' weather data.
#' 
#' Evaluation is done by identifying the longest conineous NA-free period in the
#' target weather data and inserting p_missing amount of NA in the weather observations.
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
#' @param additional_args list with all the other arguments required by the
#' patching function. elements of list need to have same name as arguments
#' of the patching function
#' @param method_patches_everything flag, indicates if the method returns all
#' weather stations or just the target station. This flag is included, so that
#' patch_flexible_several_stations is also compatible with patching methods
#' like patch_mice or patch_amelia, which return all patched weather stations
#' at one function call
#' @param period numerical vector of length two, row numbers of evaluation period, 
#' if the user wants other evaluation period than the longest consecutive NA-free
#' period per station in weather. 
#' @param return_data character, indicates what the function returns after successfull
#' evaluation. Valid options are "only_new_imputed", which returns only the original and
#' newly imputed observations per weather station, "evaluation_period" which returns
#' the whole evaluation period including non-imputed observations and "everything"
#' which returns a data frame with the same amount as \code{weather}
#' @return see the description in \code{return_data}
#' @examples #think of example here
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
get_eval_one_station <- function(weather, weather_info, target, 
                                 patch_methods, p_missing, 
                                 additional_args, 
                                 method_patches_everything = F,
                                 period = NA, return_data = 'only_new_imputed'){
  
  #method patches everything needs to be of the same length as method
  
  
  #check if weather data contains columns of Date, Year, Month, Day
  if (!("Year" %in% colnames(weather) & "Month" %in% 
        colnames(weather) & "Day" %in% colnames(weather) & "Date" %in% colnames(weather))) 
    stop("Required input column 'Year', 'Month', 'Day' and/or 'Date' is missing.")
  
  
  ######
  #check period data
  ######
  
  #in case no period provided, in which holes should be punched
  if(length(period) == 1){
    if(is.na(period)){
      
      #apply the search of NA free period to all target stations
      #as a result have a list of the periods, in the order of the stations
      period_list <- lapply(target, function(x){
        
        #check if weather station has missing data
        if(sum(is.na(weather[x])) == 0){
          
          #in that case the period covers the whole dataframe
          return(c(1, nrow(weather)))
        } else {
          
          #look for the longest period without na
          longest_coverage <- na.contiguous(weather[,x])
          
          #get the row number of start and end of longest continous period
          period <- tsp(longest_coverage)
          
          return(period)
        }
      })
      
      
      
    } else if(is.list(period)){
      #case that there is a listof length = 1 and 
      period_list <- period[[1]]
    } else{
      error('Period in wrong format')
    }
  } else{
    #check if provided period is of right format:
    #can be supplied as list, but then it should have the same amount of elements as in target
    if(is.list(period)){
      if(length(period) != length(target)){
        stop('If period is supplied in form of list, then it should have the same length as target')
        
        #check the first element of period, if stored as list, should be of length two and should be numeric
      } else if(length(period[[1]]) != 2 | is.numeric(period[[1]]) == F){
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
  period_list <-  mapply(function(x,y){c(x,y)}, period_list, target, SIMPLIFY = F)
  
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
  
  #dataframe indicating if new hole created per weather station targeted
  holes_df <- dplyr::select(mod_weather, -all_of(target))
  
  #add date column
  holes_df <- cbind(Year = weather$Year, Month = weather$Month, Day = weather$Day,
                    Date = weather$Date, holes_df)
  
  
  
  
  #####
  #run patching methods
  #####
  
  #problem: if I want to run this function, then I need also a list of list with 
  #additional arguments
  
  #I make evaluation only for one station but have model call for all stations, that doesnt make sense!
  #waste of computing time
  
  
  #there is a problem in patch several flexible: prblem when combining the model outcomes of everything patched and itereatively patched
  
  #bind everything together in one list, per elemt have the items of additional arguments, method_patches_everything and method in one item
  prep_data <- mapply(function(x,y,z){
    list(method = x, additional_args = y, method_patches_everything = z)
  }, patch_methods, additional_args, method_patches_everything, SIMPLIFY = F)
  
  
  #run patching function on evaluation data
  patched <- lapply(prep_data, function(x){
    
    #give info which method is now used
    print(paste0('Start patching method: ', x[['method']]))
    
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
    period_vec <- rep(F,nrow(weather_eval))
    period_vec[as.numeric(x[1]):as.numeric(x[2])] <- T
    
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