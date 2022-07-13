#' Quality control of daily weather data after Costa et al. (2021)
#' 
#' Performs a series of consistency tests and returns flags of suspicious data.
#' 
#' This weather quality control function was written following a the guidelines of a 
#' weather control algorithm proposed by \insertCite{costa_gap_2021;textual}{weatherQC}.
#' It consists of five consistency tests, namely:
#' 
#' \itemize{
#'  \item{Test 1: Fixed limit test; see also \code{\link{test_fixed_limit}}}
#'  \item{Test 2: Variable limit test; see also \code{\link{test_variable_limit}}}
#'  \item{Test 3: Temporal consistency test; see also \code{\link{test_temporal_continuity}}}
#'  \item{Test 4: Test for consistency among variables; see also \code{link{test_temperature_consistency}}}
#'  \item{Test 5: Spatial consistency test; see also \code{\link{test_spatial_consistency}} and \code{\link{test_precipitation_spatial_corroboration}}}
#' }
#' 
#' The function can be applied to temperature and precipitation data, though the 
#' test concerning consistency among variables only works for minimum and maximum temperature.
#' Parts of the test were also described for other weather variables like
#' relative humidity or wind speed, but this implementation allows only the test
#' to be used for minimum and maximum temperature and daily precipitation sum. Because
#' the section regarding spatial consistency was poorly described in the original 
#' article the spatial consistency test were taken from \insertCite{durre_comprehensive_2010;textual}{weatherQC}.
#' 
#' The main principle of this quality control scheme is, that at least two
#' positive tests are required for an observation to be flagged. This is intended 
#' to reduce the amount of false-positive flags. 
#' 
#' @param weather_list list of data.frames containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day", "Tmin", "Tmax", "Precip") and
#' ideally also "Tmean" for full testing power. weather_list elements names should
#' be the same as the \code{weather_info$id}
#' @param weather_info data.frame at least the columns c("id", "Longtitude",
#' "Latitude"). The column "id" should contain the same names as the named elements of
#' weather_list. Latitude and longitude should be in decimal format. Number of rows of 
#' data.frame should be same as number of elements in weather_list
#' @param aux_info data.frame listing the auxiliary weather stations. Should at least contain
#' the columns c("id", "Longitude", "Latitude")
#' @param aux_list named list of data.frames with daily weather obsrvations of auxiliary
#' weather stations. Names should be identical to \code{aux_info$id}. Strucuture of 
#' data.frames should be identical of \code{weather}. Data.frames do not necissarily
#' need to cover excat same time period as \code{weather}
#' @param mute flag to allow function to communicate testing progress in console
#' while the test runs
#' @param skip_spatial_test flag, which allows to skip the spatial tests because
#' they can be computationally demanding and may require several hours of runtime
#' @param region a character indicating the region for which the records should be downloaded. 
#' Valid options are `world` and `USA`. Can be also set as \code{NULL} in case user-defined 
#' limits are supplied
#' @param subregion a character, only needed in case \code{region} is defined. In case of 
#' \code{region = "world"} it should indicate the country the weather data is from. 
#' In case of \code{region = "USA"} it should be the state's name the weather is obtained from.
#' If user-defined limits are used, should be set to \code{NULL}
#' @param records by default set to \code{NULL}. In case of user-defined limits, it should
#' be a numeric vector of length two, containing lower and upper limits of tested variable.
#' Needs to of length 2, first value indicates lower limit and second the upper limit
#' @param probs_variable_limit percentile used as a testing threshold for variable limit test
#' @param probs_temporal_continuity percentile uded as a testing threshold for 
#' temporal conitnuity test
#' @param probs_temperature_consistency percentile used as a testing threshold
#' for temperature consistency test
#' @param max_dist maximum distance in kilometers of neighbouring stations to target station to be 
#' included in the spatial corroboration test
#' @param window_width amount of extra days added to the target and auxiliary
#' weather station for the linear regression. Extra days only part of the model
#' construction, not the testing
#' @param min_coverage minimum amount of shared observations of target and
#' auxiliary weather stations, so that auxiliary weather station is considered
#' for the test
#' @param min_correlation minimum correlation of model output x target station for 
#' an auxiliary weather station to be considered in the test
#' @param min_station minimum amount of neighbouring stations for the test. If less
#' is available, then test is not carried out and automatically returns \code{FALSE}
#' for every observation
#' @param max_station maximum number of neighbouring stations included in the test.
#' If more auxiliary stations available than \code{max_station}, then closest ones
#' are taken
#' @param max_res testing threshold, highest regular resiudal tolerated by test
#' @param max_res_norm testing threshold, highest standardized residual tolerated
#' by the test. Note: both thresholds need to be exceeded in order for the 
#' test to yield a flag
#' @return data.frame with \code{nrow(weather)} rows and the same columns as
#' in weather, but with nine extra columns called c("org_Tmin","org_Tmax", "org_Precip",
#' "flag_Tmin", "flag_Tmax", "flag_Precip", "comment_Tmin", "comment_Tmax", "comment_Precip). 
#' In the columns called "org_" plus variable name the exact copies of observations 
#' as in weather can be found, completely unaltered.
#' In the original column flagged values were replaced with NA. In the same row as where
#' the original value was removed, a comment indicating which test let to the removal
#' can be found in the "flag_" plus variable name column. The numbers indicate which
#' test yielded a positive test. Only in cases of two or more positive tests at
#' the same time was the flagged observation of the weather variable removed.
#' @examples 
#' \donotrun{
#' #prepare input data
#' weather_list <- list(target_weather)
#' names(weather_list) <- target_info$id
#' 
#' #run without spatial test
#' weather_qc_costa(weather_list = weather_list, 
#' mute = TRUE, skip_spatial_test = TRUE)
#' 
#' #run with spatial test
#' weather_qc_costa(weather_list = weather_list, 
#' weather_info = target_info,
#' aux_list = neighbour_weather,
#' aux_info = neighbour_info,
#' mute = TRUE)
#' 
#' #run with spatial test and region-specific test limit
#' weather_qc_costa(weather_list = weather_list, 
#' weather_info = target_info,
#' aux_list = neighbour_weather,
#' aux_info = neighbour_info,
#' region = 'USA', subregion = 'California',
#' mute = TRUE)
#' 
#' }
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
weather_qc_costa <- function(weather_list, 
                             weather_info = NULL, 
                             mute = FALSE, 
                             skip_spatial_test = FALSE,
                             aux_list = NULL, aux_info = NULL, 
                             region = NULL, subregion = NULL, 
                             records = NULL,
                             probs_variable_limit = c(0.01, 0.99),
                             probs_temporal_continuity = 0.995,
                             probs_temperature_consistency = 0.99,
                             max_dist = 75, window_width = 15, 
                             min_coverage = 40, min_correlation = 0.8,
                             min_station = 3, max_station = 7, max_res = 8, 
                             max_res_norm = 4){
  
  #avoid note cmd check
  . <- NULL
  
  #check if needed columns are present
  column_check <- purrr::map_lgl(weather_list, function(x){
    !all(c("Year", "Month", "Day", "Tmin", "Tmax", "Tmean", "Precip") %in% colnames(x))
  })
  if(any(column_check)){
    stop("at least one weather station in weather_list does not contain the required columns c('Year', 'Month', 'Day', 'Tmin', 'Tmax', 'Tmean', 'Precip')")
  }
  
  if(skip_spatial_test == F){
    #make sure that all the names of weather info are also found in weather_list
    if(!all(weather_info$id %in% names(weather_list))){
      stop("Could not find all weather station ids mentioned in weather_info also
         in names(weather_list")
    }
  }

  
  #add a column to weather_list objects indicating which test performed positive
  #also add Date and doy
  weather_list <- purrr::map(weather_list, function(x){
    
    if("Date" %in% colnames(x) == F){
      x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d")
    }
    
    tibble::tibble(x, 'Tmin_org' = x$Tmin, 'Tmax_org' = x$Tmax, 
                   'Precip_org' = x$Precip,'flag_Tmin' = NA, 
                   'flag_Tmax' = NA, 'flag_Precip' = NA) %>%
      dplyr::mutate(doy = lubridate::yday(.data$Date)) %>%
      dplyr::relocate(.data$Date, .data$doy)#make sure date and doy are in beginning of columns
    
  }) 
  
  if(is.null(aux_list) == F){
    #make sure aux list contains tibbles and date and doy column
    aux_list <- purrr::map(aux_list, function(x){
      tibble::tibble(x, 'Date' = as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d")) %>%
        dplyr::mutate(doy = lubridate::yday(.data$Date))
    })
  }


  

  #do testing of input stuff: 
  #   weather needs to have certain columns
  #   weather coords needs to be of length two and numeric
  #   variable should be tmin, tmax (or preciptiation)
  #   aux_list needs to be list, names need to be same as in aux_info$id; colnames should contain same objects as weather does
  #   aux_info needs to contain coordinates and date?
  
  #### Fixed limit test ####
  
  if(mute == FALSE){
    cat(paste0(rep('-', 10), recycle0 = FALSE), '\n')
    cat('Fixed limit test', '\n')
    cat('', '\n')
  }
  
  #apply test, add column with flag
  weather_list <- purrr::map(weather_list, function(x){
    x$fixed_lim_test_Tmin <- test_fixed_limit(weather = x, variable = 'Tmin', region = region,
                     subregion = subregion, records = records)
    
    x$fixed_lim_test_Tmax <- test_fixed_limit(weather = x, variable = 'Tmax', region = region,
                                              subregion = subregion, records = records)
    
    x$fixed_lim_test_Precip <- test_fixed_limit(weather = x, variable = 'Precip', region = region,
                                                subregion = subregion, records = records)
    
    return(x)
  })
  

  

  #### Variable limit test ####
  
  if(mute == FALSE){
    cat(paste0(rep('-', 10), recycle0 = FALSE), '\n')
    cat('Variable limit test', '\n')
    cat('', '\n')
  }
  
  #tmin
  weather_list <- purrr::map(weather_list, function(x){
    x$variable_lim_test_Tmin <- test_variable_limit(weather = x, variable = 'Tmin', 
                                                    probs = probs_variable_limit)
    
    x$variable_lim_test_Tmax <- test_variable_limit(weather = x, variable = 'Tmax', 
                                                    probs = probs_variable_limit)
    
    x$variable_lim_test_Precip <- test_variable_limit(weather = x, variable = 'Precip', 
                                                      probs = probs_variable_limit)
    
    return(x)
  })
  

  
  
  #### Temporal consistency ####
  
  if(mute == FALSE){
    cat(paste0(rep('-', 10), recycle0 = FALSE), '\n')
    cat('Temporal consistency test', '\n')
    cat('', '\n')
  }
  

  weather_list <- purrr::map(weather_list, function(x){
    x$temporal_consistency_Tmin <- test_temporal_continuity(weather = x, variable ='Tmin', 
                                                         prob = probs_temporal_continuity)
    
    x$temporal_consistency_Tmax <- test_temporal_continuity(weather = x, variable ='Tmax', 
                                                            prob = probs_temporal_continuity)
    
    x$temporal_consistency_Precip <- test_temporal_continuity(weather = x, variable ='Precip', 
                                                              prob = probs_temporal_continuity)
    
    return(x)
  })
  


  
  #### Consistency of variables ####
  
  if(mute == FALSE){
    cat(paste0(rep('-', 10), recycle0 = FALSE), '\n')
    cat('Test consistency of variables', '\n')
    cat('', '\n')
  } 

  weather_list <- purrr::map(weather_list, function(x){
    #temperature
    x$consistency_variable_Tmin <- x$consistency_variable_Tmax <- test_temperature_consistency(weather = x,
                                                                                               probs = probs_temperature_consistency)
    #precipitation                                                                                                            
    x$consistency_variable_Precip <- FALSE
    
    return(x)
  })
  
  
  #### Spatial consistency ####
  
  #flag which allows to skip spatial tests
  if(is.null(aux_info) == TRUE | is.null(aux_list) == TRUE){
    skip_spatial_test <- TRUE
    warning("Because arguments aux_info and aux_list were not provided, the spatial
            consistency tests are skipped")
  }
  
  
  if(skip_spatial_test == FALSE){
    
    if(mute == FALSE){
      cat(paste0(rep('-', 10), recycle0 = FALSE), '\n')
      cat('Spatial consistency', '\n')
      cat('', '\n')
    } 
    
    weather_list <- purrr::map(weather_info$id, function(x){
      
      weather_coords <- c(weather_info$Longitude[weather_info$id == x],
                          weather_info$Latitude[weather_info$id == x])
      
      weather_list[[x]]$spatial_test_Tmin <- test_spatial_consistency(weather = weather_list[[x]], 
                                                      weather_coords = weather_coords, 
                                                      aux_list = aux_list, 
                                                      aux_info = aux_info, 
                                                      variable = 'Tmin', 
                                                      max_dist = max_dist, 
                                                      window_width = window_width, 
                                                      min_coverage = min_coverage, 
                                                      min_correlation = min_correlation,
                                                      min_station = min_station, 
                                                      max_station = max_station, 
                                                      max_res = max_res, 
                                                      max_res_norm = max_res_norm)
      
      weather_list[[x]]$spatial_test_Tmax <- test_spatial_consistency(weather = weather_list[[x]], 
                                                      weather_coords = weather_coords, 
                                                      aux_list = aux_list, 
                                                      aux_info = aux_info, 
                                                      variable = 'Tmax', 
                                                      max_dist = max_dist, 
                                                      window_width = window_width, 
                                                      min_coverage = min_coverage, 
                                                      min_correlation = min_correlation,
                                                      min_station = min_station, 
                                                      max_station = max_station, 
                                                      max_res = max_res, 
                                                      max_res_norm = max_res_norm)
      
      
      weather_list[[x]]$spatial_test_Precip <- test_precipitation_spatial_corroboration(weather = weather_list[[x]], 
                                                                      weather_coords = weather_coords,
                                                                      aux_info = aux_info,
                                                                      aux_list = aux_list,
                                                                      max_dist = max_dist,
                                                                      max_station = max_station,
                                                                      min_station = min_station)
      
      return(weather_list[[x]])
    })
    
  } else{
    
    #case that spatial test was skipped, still add flag so that function
    #can operate normally
    
    weather_list <- purrr::map(weather_list, function(x){
      x$spatial_test_Tmin <- x$spatial_test_Tmax <- x$spatial_test_Precip <- FALSE
      return(x)
    })
  }
  
 
  #### Summarise the test ####
  
  #add final flag
  #add comment which tests were positive
  #remove flagged data
  weather_list <- purrr::map(weather_list, function(x){
    
    x$flag_Tmin <- rowSums(x[,c("fixed_lim_test_Tmin", "variable_lim_test_Tmin",
                                "temporal_consistency_Tmin", "consistency_variable_Tmin",
                                "spatial_test_Tmin")], na.rm = TRUE) >= 2
    x$Tmin[x$flag_Tmin] <- NA
    x$flag_Tmin <- paste0(ifelse(x$fixed_lim_test_Tmin, yes = '1, ', no = ''), 
                             ifelse(x$variable_lim_test_Tmin, '2, ', ''),
                             ifelse(x$temporal_consistency_Tmin, '3, ', ''),
                             ifelse(x$consistency_variable_Tmin, '4, ', ''),
                             ifelse(x$spatial_test_Tmin, '5, ', '')) %>%
      trimws(which = 'right') %>% #trim trailing white space
      gsub(pattern = "\\,$", replacement = "", x = .) #trim trailing commata
    
    x$flag_Tmax <- rowSums(x[,c("fixed_lim_test_Tmax", "variable_lim_test_Tmax",
                                "temporal_consistency_Tmax", "consistency_variable_Tmax",
                                "spatial_test_Tmax")], na.rm = TRUE) >= 2
    x$Tmax[x$flag_Tmax] <- NA
    x$flag_Tmax <- paste0(ifelse(x$fixed_lim_test_Tmax, yes = '1, ', no = ''), 
                             ifelse(x$variable_lim_test_Tmax, '2, ', ''),
                             ifelse(x$temporal_consistency_Tmax, '3, ', ''),
                             ifelse(x$consistency_variable_Tmax, '4, ', ''),
                             ifelse(x$spatial_test_Tmax, '5, ', '')) %>%
      trimws(which = 'right') %>% #trim trailing white space
      gsub(pattern = "\\,$", replacement = "", x = .) #trim trailing commata
    
    x$flag_Precip <- rowSums(x[,c("fixed_lim_test_Precip", "variable_lim_test_Precip",
                                  "temporal_consistency_Precip", "consistency_variable_Precip",
                                  "spatial_test_Precip")], na.rm = TRUE) >= 2
    x$Precip[x$flag_Precip] <- NA
    x$flag_Precip <- paste0(ifelse(x$fixed_lim_test_Precip, yes = '1, ', no = ''), 
                               ifelse(x$variable_lim_test_Precip, '2, ', ''),
                               ifelse(x$temporal_consistency_Precip, '3, ', ''),
                               ifelse(x$consistency_variable_Precip, '4, ', ''),
                               ifelse(x$spatial_test_Precip, '5, ', '')) %>%
      trimws(which = 'right') %>% #trim trailing white space
      gsub(pattern = "\\,$", replacement = "", x = .) #trim trailing commata
    
    #drop old flag columns
    x <- x %>%
      dplyr::select(-c(.data$fixed_lim_test_Tmin, 
                       .data$variable_lim_test_Tmin,
                       .data$temporal_consistency_Tmin, 
                       .data$consistency_variable_Tmin,
                       .data$spatial_test_Tmin, 
                       .data$fixed_lim_test_Tmax, 
                       .data$variable_lim_test_Tmax,
                       .data$temporal_consistency_Tmax, 
                       .data$consistency_variable_Tmax,
                       .data$spatial_test_Tmax,
                       .data$fixed_lim_test_Precip, 
                       .data$variable_lim_test_Precip,
                       .data$temporal_consistency_Precip, 
                       .data$consistency_variable_Precip,
                       .data$spatial_test_Precip))
    
    return(x)
  })
  

  return(weather_list)
}
