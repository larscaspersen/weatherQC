#' Quality control of daily weather data after Durre et al. (2010)
#' 
#' Performs a series of consistency tests and returns flags of suspicious 
#' daily weather data.
#' 
#' This weather quality control function was written following a the guidelines of a 
#' weather control algorithm proposed by \insertCite{durre_comprehensive_2010;textual}{weatherQC}.
#' In total it consists of 6 groups of tests which contain several independent 
#' tests:
#' 
#' \itemize{
#'  \item{Group 1: Basic integrity tests}
#'     \itemize{
#'     \item{Naught test \code{\link{test_naught_weather}}}
#'     \item{Duplicate test \code{\link{get_duplicated_values}}}
#'     \item{Fixed limit test \code{\link{test_fixed_limit}}}
#'     \item{Streak test \code{\link{get_streaks}}}
#'     \item{Frequent value test \code{\link{check_frequent_value}}}
#'     }
#'  \item{Group 2: Outlier tests}
#'  \itemize{
#'     \item{Gap test \code{\link{perform_gap_check}}}
#'     \item{Climatological outlier test \code{\link{perform_climate_outlier_check}}}
#'     }
#'  \item{Group 3: Internal and temporal consistency tests}
#'  \itemize{
#'     \item{Iterative temperature consistency test \code{\link{test_iterative_temperature_consistency}}}
#'     \item{Spike / dip test \code{\link{test_spike_dip}}}
#'     \item{Lagged temperature range test \code{\link{test_lagged_temperature_range}}}
#'     }
#'  \item{Group 4: Spatial consistency tests}
#'  \itemize{
#'     \item{Spatial regression test \code{\link{test_spatial_consistency}}}
#'     \item{Temperature spatial corroboration test \code{\link{test_temperature_corroboration}}}
#'     \item{Precipitation spatial corroboration test \code{\link{test_precipitation_spatial_corroboration}}}
#'     }
#'  \item{Group 5: Megaconsistency tests}
#'  \itemize{
#'     \item{Temperature megaconsistency test \code{\link{test_temperature_megaconsistency}}}
#'     }
#' }
#' 
#' The function automatically applies the tests to temperature (Tmin, Tmax) and
#' Precipitation (Precip) observations within the data.frames of weather_list. The
#' testing order matters, because flagged observations are removed from the subsequent
#' tests. This in contrast to \code{\link{weather_qc_costa}} where at least
#' two tests are needed for an observation to be flagged. The rationale behind the 
#' testing design in \code{\link{weather_qc_durre}} is that the first tests are
#' less restrictive but remove the strong outlier (like let's say -9999), which
#' could reduce the testing quality of more restrictive tests like climatological outlier
#' test. 
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
#' the columns c("id", "Longitude", "Latitude"). Can also be set to NULL, in that
#' case spatial tests are automatically skipped
#' @param aux_list named list of data.frames with daily weather observations of auxiliary
#' weather stations. Names should be identical to \code{aux_info$id}. Structure of 
#' data.frames should be identical of \code{weather}. Data.frames do not necessarily
#' need to cover excat same time period as \code{weather}. Can also be set to NULL, in that
#' case spatial tests are automatically skipped
#' @param mute flag to allow function to communicate testing progress in console
#' while the test runs
#' @param skip_spatial_test flag, which allows to skip the spatial tests because
#' they can be computationally demanding and may require several hours of runtime
#' @param duplicate_test_min_obs minimum amount of non-zero precipitation events
#' to be considered in the duplicate test. By default it is at least three non-zero
#' precipitation observations per tested time period (month or year)
#' @param same_temp_threshold threhsold for the detection of duplicated temperature
#' observations
#' @param region a character indicating the region for which the records should be downloaded. 
#' Valid options are `world` and `USA`. Can be also set as \code{NULL} in case user-defined 
#' limits are supplied
#' @param subregion a character, only needed in case \code{region} is defined. In case of 
#' \code{region = "world"} it should indicate the country the weather data is from. 
#' In case of \code{region = "USA"} it should be the state's name the weather is obtained from.
#' If user-defined limits are used, should be set to \code{NULL}
#' @param records_temp minimum and maximum temperature in degree Celsius limits for the fixed
#' limit test. If left NULL, either the function scraps records from the web if
#' \code{region} and \code{subregion} are supplied, otherwise global temperature
#' records are used
#' @param records_precip lower and upper limits of daily precipitation sum (mm) 
#' for the fixed limit test. If left NULL, either the function scraps records 
#' from the web if \code{region} and \code{subregion} are supplied, otherwise 
#' global daily precipitation sum records are used
#' @param streak_threshold test limit, if streaks this length or greater are found, 
#' then test returns positive flag
#' @param percentile_frequent_value_test precipitation percentile which will be 
#' used to to judge if frequently repeated values are to be flagged or not
#' @param min_non_zero_days minimum amount of non-zero precipitation observation need 
#' to be present for ecdf calculation, otherwise NA returned
#' @param temp_gap_threshold testing threshold for temperature data, if gaps are equal
#' or larger, then function flags data. By default 10 degree C
#' @param prec_gap_threshold testing threshold for precipitation data, if gaps are equal
#' or larger, then function flags data. By default 300 mm
#' @param max_temperature_z maximum size of standardized resiudla of temperature value
#' to climatological mean for the day of interest
#' @param max_prec_threshold factor for test: \code{precipitation >= max_prec_threshold *
#' precipitation percentile} under non-freezing conditions
#' @param max_prec_threshold_freezing factor for test: \code{precipitation >= max_prec_threshold *
#' precipitation percentile}  under freezing-conditions
#' @param prec_percentile_climate_outlier precipitation percentile used for the comparison
#' @param dip_threshold threshold for the test, spike and dips lower than this value
#' are ignored
#' @param lagged_range_max_diff threshold for the test, spike and dips lower than this value
#' are ignored
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
#' @param window_width amount of extra days added to the target and auxiliary
#' weather station for the linear regression. Extra days only part of the model
#' construction, not the testing
#' @param max_diff_temp_corroboration maximum difference of the lowest minimum difference of target observation
#' to neighbouring observations for temperature anomalies
#' @param min_obs_megaconsistency minimum amount of observation per calendar month in order 
#' to be considered in the test, otherwise for the calendar month all observations
#' automatically get flagged as FALSE
#' @return data.frame with \code{nrow(weather)} rows and the same columns as
#' in weather, but with six extra columns called c("org_Tmin","org_Tmax", "org_Precip",
#' "flag_Tmin", "flag_Tmax", "flag_Precip"). In the columns called "org_" plus variable
#' name the exact copies of observations as in weather can be found, completely unaltered.
#' In the original column flagged values were replaced with NA. In the same row as where
#' the original value was removed, a comment indicating which test let to the removal
#' can be found in the "flag_" plus variable name column.
#' @examples weather_qc_durre(weather_list = list(target_weather), 
#' skip_spatial_test = TRUE, mute = TRUE)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt 
#' @import magrittr
#' @references
#' \insertAllCited{}
#' @export
weather_qc_durre <- function(weather_list, 
                             weather_info = NULL,
                             aux_list = NULL, 
                             aux_info = NULL,
                             mute = TRUE, 
                             skip_spatial_test = FALSE,
                             duplicate_test_min_obs = 3,
                             same_temp_threshold = 10,
                             region = NULL, 
                             subregion = NULL,
                             records_temp = NULL,
                             records_precip = NULL,
                             streak_threshold = 20,
                             percentile_frequent_value_test = c(.3, .5, .7, .9),
                             min_non_zero_days = 20,
                             temp_gap_threshold = 10,
                             prec_gap_threshold = 300,
                             max_temperature_z = 6,
                             max_prec_threshold = 9,
                             max_prec_threshold_freezing = 5,
                             prec_percentile_climate_outlier = 0.95,
                             dip_threshold = 25,
                             lagged_range_max_diff = 40,
                             max_dist = 75,
                             window_width = 15,
                             min_coverage = 40,
                             min_correlation = 0.8,
                             min_station = 3,
                             max_station = 7,
                             max_res = 8,
                             max_res_norm = 4,
                             max_diff_temp_corroboration = 10,
                             min_obs_megaconsistency = 140){
  
  #add a column to weather_list objects indicating which test performed positive
  #also add Date and doy
  weather_list <- purrr::map(weather_list, function(x){
    
    if("Date" %in% colnames(x) == F){
      x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d")
    }
    
    tibble::tibble(x, 'Tmin_org' = x$Tmin, 'Tmax_org' = x$Tmax, 'Tmean_org' = x$Tmean,
                   'Precip_org' = x$Precip,'flag_Tmin' = NA, 
                   'flag_Tmax' = NA, 'flag_Tmean' = NA,'flag_Precip' = NA) %>%
      dplyr::mutate(doy = lubridate::yday(Date)) %>%
      dplyr::relocate(Date, doy)#make sure date and doy are in beginning of columns
    
  }) 
  
  #if the user does not provide any information on auxilliary weather stations, 
  #then spatial consistency tests are skipped
  
  if(is.null(aux_info) == T | is.null(aux_list) == T){
    skip_spatial_test <- TRUE
    warning("Because arguments aux_info and aux_list were not provided, the spatial
            consistency tests are skipped")
  }
  
  ####
  #basic integrity check
  ####
  if(mute == FALSE){
    cat(paste0(rep('-', 10), recycle0 = FALSE), '\n')
    cat('Basic integrity checks', '\n')
    cat('', '\n')
  }
  
  if(mute == FALSE){
    cat('Naught check', '\n')
  }
  #1: naught check
  weather_list <- purrr::map(weather_list, function(x){
    #Tmin
    x <- clear_flagged_data(weather = x, 
                       variable = 'Tmin', 
                       test_result = test_naught_weather(weather = x),
                       test_name = 'naught_check')
    #Tmax
    x <- clear_flagged_data(weather = x, 
                            variable = 'Tmax', 
                            test_result = test_naught_weather(weather = x),
                            test_name = 'naught_check')
    
    return(x)
    
  })

  
  #2 duplicate check
  if(mute == FALSE){
    cat('Duplicate Check', '\n')
  }
  #between entire years only for precipitation
  #tmin
  weather_list <- purrr::map(weather_list, function(x){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                            test_result = get_duplicated_values(weather = x, 
                                                                variable = 'Tmin', 
                                                                precip_min_nonzero = duplicate_test_min_obs,
                                                                same_temp_threshold = same_temp_threshold), 
                            test_name = 'duplicated')
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                            test_result = get_duplicated_values(weather = x, variable = 'Tmax', 
                                                                precip_min_nonzero = duplicate_test_min_obs,
                                                                same_temp_threshold = same_temp_threshold), 
                            test_name = 'duplicated')
    #Precipitation
    x <- clear_flagged_data(weather = x, variable = 'Precip', 
                            test_result = get_duplicated_values(weather = x, variable = 'Precip', 
                                                                precip_min_nonzero = duplicate_test_min_obs,
                                                                same_temp_threshold = same_temp_threshold), 
                            test_name = 'duplicated')
    return(x)
  })
  

  #### record exceedance test
  if(mute == FALSE){
    cat('Record exceedance test', '\n')
  }
  
  #if region and subregion are provided, download data
  if(is.null(region) == FALSE & is.null(subregion) == FALSE){
    #download records
    records <- get_weather_records(region = region) %>%
      dplyr::filter(.data$Country == subregion)
    
    records_temp <- c(records$Tmin, records$Tmax)
    records_precip <- c(0, records$Precip)
  } 
  
  #carry out test
  weather_list <- purrr::map(weather_list, function(x){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = test_fixed_limit(weather = x, 
                                                      variable = 'Tmin',
                                                      region = NULL,
                                                      subregion = NULL,
                                                      records = records_temp), 
                       test_name = 'record_exceedance')
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                            test_result = test_fixed_limit(weather = x, 
                                                           variable = 'Tmax',
                                                           region = NULL,
                                                           subregion = NULL,
                                                           records = records_temp), 
                            test_name = 'record_exceedance')
    
    #Tmean, if present
    if('Tmean' %in% colnames(x)){
       x <- clear_flagged_data(weather = x, variable = 'Tmean', 
                         test_result = test_fixed_limit(weather = x, 
                                                        variable = 'Tmean',
                                                        region = NULL,
                                                        subregion = NULL,
                                                        records = records_temp), 
                         test_name = 'record_exceedance')
    }
    
    #Precip
        x <- clear_flagged_data(weather = x, variable = 'Precip', 
                              test_result = test_fixed_limit(weather = x, 
                                                             variable = 'Precip',
                                                             region = NULL,
                                                             subregion = NULL,
                                                             records = records_precip), 
                              test_name = 'record_exceedance')
    return(x)
  })
  



  #identical value streak test
  if(mute == FALSE){
    cat('Value Streak Test', '\n')
  }
  
  #Tmin
  weather_list <- purrr::map(weather_list, function(x){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = get_streaks(weather = x, 
                                                 variable = 'Tmin',
                                                 rep_threshold = streak_threshold), 
                       test_name = 'streaks')
    
    #Tmax
    x <-     clear_flagged_data(weather = x, variable = 'Tmax', 
                                test_result = get_streaks(weather = x, 
                                                          variable = 'Tmax',
                                                          rep_threshold = streak_threshold), 
                                test_name = 'streaks')
    return(x)
  })
  
  
  #calculate percentiles for each weather df, store in list
  prec_percentile_list <- purrr::map(weather_list, function(x){
    get_each_day_precipitation_percentile(weather = x, 
                                          probs = percentile_frequent_value_test)
  })
  
  if(mute == FALSE){
    cat('Frequent Identical Value Test', '\n')
  }
  
  weather_list <- purrr::map2(weather_list, prec_percentile_list, function(x,y){
    clear_flagged_data(weather = x, variable = 'Precip', 
                       test_result = check_frequent_value(weather = x, 
                                                          percentile_df = y,
                                                          min_non_zero_days = min_non_zero_days),
                       test_name = 'frequent_ident_value')
  })
  
  
  ####
  #outlier checks
  ####
  if(mute == FALSE){
    cat(rep('-', 10), '\n')
    cat('Outlier checks', '\n')
    cat('', '\n')
  }
  
  #gap check
  if(mute == FALSE){
    cat('Gap check', '\n')
  }

  weather_list <- purrr::map(weather_list, function(x){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = perform_gap_check(weather = x, 
                                                       variable = 'Tmin', 
                                                       temp_gap_threshold = temp_gap_threshold,
                                                       prec_gap_threshold = prec_gap_threshold), 
                       test_name = 'gap_check')
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                            test_result = perform_gap_check(weather = x, 
                                                            variable = 'Tmax', 
                                                            temp_gap_threshold = temp_gap_threshold,
                                                            prec_gap_threshold = prec_gap_threshold), 
                            test_name = 'gap_check')
    
    #Tmean, if present
    if('Tmean' %in% colnames(x)){
      x <- clear_flagged_data(weather = x, variable = 'Tmean', 
                         test_result = perform_gap_check(weather = x, 
                                                         variable = 'Tmean', 
                                                         temp_gap_threshold = temp_gap_threshold,
                                                         prec_gap_threshold = prec_gap_threshold), 
                         test_name = 'gap_check')
    }
    #Precipitation
    x <- clear_flagged_data(weather = x, variable = 'Precip', 
                            test_result = perform_gap_check(weather = x, 
                                                            variable = 'Precip', 
                                                            temp_gap_threshold = temp_gap_threshold,
                                                            prec_gap_threshold = prec_gap_threshold), 
                            test_name = 'gap_check')
    return(x)
  })
  


  
  #climatological outlier
  if(mute == FALSE){
    cat('Climatological Outlier Test', '\n')
  }

  weather_list <- purrr::map(weather_list, function(x){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = perform_climate_outlier_check(weather = x, 
                                                                   variable = 'Tmin',
                                                                   max_temperature_z = max_temperature_z,
                                                                   max_prec_threshold = max_prec_threshold,
                                                                   max_prec_threshold_freezing = max_prec_threshold_freezing,
                                                                   prec_percentile = prec_percentile_climate_outlier
                       ), 
                       test_name = 'clim_outlier')
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                            test_result = perform_climate_outlier_check(weather = x, 
                                                                        variable = 'Tmax',
                                                                        max_temperature_z = max_temperature_z,
                                                                        max_prec_threshold = max_prec_threshold,
                                                                        max_prec_threshold_freezing = max_prec_threshold_freezing,
                                                                        prec_percentile = prec_percentile_climate_outlier), 
                            test_name = 'clim_outlier')
    
    #Tmean, if present
    if('Tmean' %in% colnames(x)){
      x <- clear_flagged_data(weather = x, variable = 'Tmean', 
                         test_result = perform_climate_outlier_check(weather = x, 
                                                                     variable = 'Tmean',
                                                                     max_temperature_z = max_temperature_z,
                                                                     max_prec_threshold = max_prec_threshold,
                                                                     max_prec_threshold_freezing = max_prec_threshold_freezing,
                                                                     prec_percentile = prec_percentile_climate_outlier
                         ), 
                         test_name = 'clim_outlier')
    }
    #Precipitation
    x <- clear_flagged_data(weather = x, variable = 'Precip', 
                            test_result = perform_climate_outlier_check(weather = x, 
                                                                        variable = 'Precip',
                                                                        max_temperature_z = max_temperature_z,
                                                                        max_prec_threshold = max_prec_threshold,
                                                                        max_prec_threshold_freezing = max_prec_threshold_freezing,
                                                                        prec_percentile = prec_percentile_climate_outlier), 
                            test_name = 'clim_outlier')
    return(x)
  })
  

  
  ######
  #Temporal consistency checks
  ######
  if(mute == FALSE){
    cat(rep('-', 10), '\n')
    cat('Temporal Consistency checks', '\n')
    cat('', '\n')
  }
  
  #iterative temperature consistency
  if(mute == FALSE){
    cat('Iterative Consistency Check', '\n')
  }
  
  #run the temporal temperature consistency test only once
  test_list <- purrr::map(weather_list, function(x){
    test_iterative_temperature_consistency(weather = x)
  })
  
  #apply resulst to tmin
  weather_list <- purrr::map2(weather_list, test_list, function(x,y){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', test_result = y$tmin_flag, 
                       test_name = 'iterative_consistency')
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', test_result = y$tmax_flag, 
                            test_name = 'iterative_consistency')
    return(x)
    
    #Tmean, if present
    if('Tmean' %in% colnames(x)){
      x <- clear_flagged_data(weather = x, variable = 'Tmean', 
                         test_result = y$tmean_flag, 
                         test_name = 'iterative_consistency')
    }
    return(x)
  })
  


  #spike - dip check
  if(mute == FALSE){
    cat('Spike/Dip Test', '\n')
  }

  weather_list <- purrr::map(weather_list, function(x){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = test_spike_dip(weather = x, 
                                                    variable = 'Tmin',
                                                    dip_threshold = dip_threshold), 
                       test_name = 'spike-dip')
    
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                            test_result = test_spike_dip(weather = x, 
                                                         variable = 'Tmax',
                                                         dip_threshold = dip_threshold), 
                            test_name = 'spike-dip')
    #Tmean, if present
    if('Tmean' %in% colnames(x)){
      x <- clear_flagged_data(weather = x, variable = 'Tmean', 
                              test_result = test_spike_dip(weather = x, 
                                                           variable = 'Tmean',
                                                           dip_threshold = dip_threshold), 
                              test_name = 'spike-dip')
    }
    return(x)
  })

  
  #lagged temperature range check
  if(mute == FALSE){
    cat('Lagged Temperature Range Test', '\n')
  }
  test_list <- purrr::map(weather_list, function(x){
    test_lagged_temperature_range(weather = x, 
                                  max_diff = lagged_range_max_diff)
  })
  
  weather_list <- purrr::map2(weather_list, test_list, function(x,y){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = y$tmin_flag, 
                       test_name = 'lagged_temperature')
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                            test_result = y$tmax_flag, 
                            test_name = 'lagged_temperature')
    return(x)
  })
  

  
  #####
  #spatial consistency check
  #####
  
  #this switch allows to skip spatial consistency tests, because they can
  #be quite slow
  if(skip_spatial_test == FALSE){
    if(mute == FALSE){
      cat(rep('-', 10), '\n')
      cat('Spatial Consistency checks', '\n')
      cat('(these usually take a little bit longer)', '\n')
      cat('', '\n')
    }
    
    #make sure weather_info is of same order as weather_list
    weather_info <- weather_info[match(weather_info$id, names(weather_list)), ]
    
    #add cleaned weather_list to aux_list and make sure there are no duplicates
    aux_list[weather_info$id] <- weather_list
    
    #make sure that each element of aux_list is a tibble and contains date
    aux_list <- purrr::map(aux_list, function(x){
      if(("Date" %in% colnames(x)) == F){
        #add date
        x$Date = as.Date(paste(x$Year, x$Month, x$Day, sep = "-"),
                         format = "%Y-%m-%d")
      }
      
      tibble::tibble(x)})
    
    if(mute == FALSE){
      cat('Spatial Regression Test', '\n')
    }
    
    weather_list <- purrr::imap(weather_list, function(x,id){
      #Tmin
      x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                         test_result = test_spatial_consistency(weather = x, 
                                                                weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                   weather_info$Latitude[weather_info$id == id]),
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
                                                                max_res_norm = max_res_norm), 
                         test_name = 'spatial_regression')
      #Tmax
      x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                              test_result = test_spatial_consistency(weather = x, 
                                                                     weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                        weather_info$Latitude[weather_info$id == id]),
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
                                                                     max_res_norm = max_res_norm), 
                              test_name = 'spatial_regression')
      return(x)
    })
    

    
    #temperature corroboration
    if(mute == FALSE){
      cat('Spatial corroboration Test', '\n')
    }

    weather_list <- purrr::imap(weather_list, function(x,id){
      #Tmin
      x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                         test_result = test_temperature_corroboration(weather = x, 
                                                                    weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                       weather_info$Latitude[weather_info$id == id]),
                                                                    aux_list = aux_list, 
                                                                    aux_info = aux_info, 
                                                                    variable = 'Tmin',
                                                                    max_station = max_station,
                                                                    min_station = min_station,
                                                                    max_dist = max_dist,
                                                                    max_diff = max_diff_temp_corroboration), 
                         test_name = 'spatial_corroboration')
      #Tmax
      x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                              test_result = test_temperature_corroboration(weather = x, 
                                                                           weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                              weather_info$Latitude[weather_info$id == id]),
                                                                           aux_list = aux_list, 
                                                                           aux_info = aux_info, 
                                                                           variable = 'Tmax',
                                                                           max_station = max_station,
                                                                           min_station = min_station,
                                                                           max_dist = max_dist,
                                                                           max_diff = max_diff_temp_corroboration), 
                              test_name = 'spatial_corroboration')
      #Precipitation
      x <- clear_flagged_data(weather = x, variable = 'Precip', 
                              test_result = test_precipitation_spatial_corroboration(weather = x, 
                                                                                     weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                                        weather_info$Latitude[weather_info$id == id]),
                                                                                     aux_list = aux_list, 
                                                                                     aux_info = aux_info,
                                                                                     max_dist = max_dist,
                                                                                     max_station = max_station,
                                                                                     min_station = min_station), 
                              test_name = 'spatial_corroboration')
      return(x)
    })
    
  
  
  ####
  #mega consistency check
  ####
  if(mute == FALSE){
    cat(rep('-', 10), '\n')
    cat('Megaconsistency checks', '\n')
    cat('', '\n')
  }
  
  #extremes mega consistency check
  if(mute == FALSE){
    cat('Temperature Megaconsistency Test')
  }
  test <- purrr::map(weather_list, function(x){
    test_temperature_megaconsistency(weather = x,
                                     min_obs = min_obs_megaconsistency)
  })
  
  weather_list <- purrr::map2(weather_list, test, function(x,y){
    #Tmin
    x <- clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = y$flag_tmin, 
                       test_name =  'mega_consistency')
    #Tmax
    x <- clear_flagged_data(weather = x, variable = 'Tmax', 
                            test_result = y$flag_tmax, 
                            test_name =  'mega_consistency')
    return(x)
  })
  
  }
  
  #tests are complete :)

  return(weather_list)
}