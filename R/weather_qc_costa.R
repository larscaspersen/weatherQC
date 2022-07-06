#' Quality control of daily weather data after Costa et al. (2021)
#' 
#' Performs a series of consistency tests and returns flags of suspicious data.
#' 
#' This weather quality control function was written following a the guidlines of a 
#' weather control algorithm proposed by \insertCite{costa_gap_2021;textual}{weatherQC}.
#' It consists of five consistency tests, namely:
#' 
#' \itemize{
#'  \item{Test 1: Fixed limit test; see also \code{link{test_fixed_limit}}}
#'  \item{Test 2: Variable limit test; see also \code{link{test_variable_limit}}}
#'  \item{Test 3: Temporal consistency test; see also \code{link{test_temporal_continuity}}}
#'  \item{Test 4: Test for consistency among variables; see also \code{link{test_temperature_consistency}}}
#'  \item{Test 5: Spatial consistency test; see also \code{link{test_spatial_consistency}} and \code{\link{test_precipitation_spatial_corrobation}}}
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
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param weather_coords numerical vector of length two. Should contain longitude and 
#' latitude (in that order) of target station in decimal format
#' @param variable column name in \code{weather} for which the test is performed. Should
#' be either Tmin or Tmax. data.frames in \code{aux_list} need to have the same
#' name
#' @param aux_info data.frame listing the auxiliary weather stations. Should at least contain
#' the columns c("id", "Longitude", "Latitude")
#' @param aux_list named list of data.frames with daily weather obsrvations of auxiliary
#' weather stations. Names should be identical to \code{aux_info$id}. Strucuture of 
#' data.frames should be identical of \code{weather}. Data.frames do not necissarily
#' need to cover excat same time period as \code{weather}
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
#' included in the spatial corrobation test
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
#' @return data.frame with \code{nrow(weather)} rows and six columns. The first
#' five columns are the individual test results and the sixth column is the
#' aggregated test resuls, which is positive if at least two tests yielded
#' positive results for an observation. All columns contain logicals, where 
#' values of \code{TRUE} indicate successful test, meaning that the tested 
#' variable exceeded the limits of the test and is flagged as suspicious
#' @examples weather_qc_costa(weather = weather, 
#' weather_coords = c(weather_info$Longitude, weather_info$Latidue),
#' variable = "Tmin", aux_list = aux_list, aux_info = aux_info)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
weather_qc_costa <- function(weather, weather_coords, variable,
                             aux_list, aux_info, region = NULL, subregion = NULL, 
                             records = NULL,
                             probs_variable_limit = c(0.01, 0.99),
                             probs_temporal_continuity = 0.995,
                             probs_temperature_consistency = 0.99,
                             max_dist = 75, window_width = 15, 
                             min_coverage = 40, min_correlation = 0.8,
                             min_station = 3, max_station = 7, max_res = 8, 
                             max_res_norm = 4){
  
  #if temperature, then Tmean is needed
  if(variable %in% c('Tmin', 'Tmax')){
    if(!'Tmean' %in% colnames(weather)){
      stop('For variable consistency test daily average temperature called "Tmean" is required')
    }
  }
  
  if(!'Date' %in% colnames(weather)){
    weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                            format = '%Y-%m-%d')
  }
  
  #make sure weather and aux_list are tibbles
  weather <- tibble(weather)
  aux_list <- map(aux_list, tibble)
  
  #make sure that weather has date column
  weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                          format = '%Y-%m-%d')
  
  #do testing of input stuff: 
  #   weather needs to have certain columns
  #   weather coords needs to be of length two and numeric
  #   variable should be tmin, tmax (or preciptiation)
  #   aux_list needs to be list, names need to be same as in aux_info$id; colnames should contain same objects as weather does
  #   aux_info needs to contain coordinates and date?
  
  #call fixed limits test
  fixed_lim <- test_fixed_limit(weather = weather, region = region, 
                                subregion = subregion,variable = variable)
  
  #call variable limits test
  variable_lim <- test_variable_limit(weather = weather, variable = variable,
                                      probs = probs_variable_limit)
  
  #call temporal consistency test
  temporal_consistency <- test_temporal_continuity(weather = weather, variable = variable, 
                                                   prob = probs_temporal_continuity)
  
  #call variable consistency test (only applicable for temperature data)
  #make ifelse condition for temperature and precipitation
  if(variable %in% c('Tmin', 'Tmax')){
    variable_consistency <- test_temperature_consistency(weather = weather,
                                                         probs = probs_temperature_consistency)
    
    spatial_consistency <- test_spatial_consistency(weather = weather, 
                                                    weather_coords = weather_coords, 
                                                    aux_list = aux_list, 
                                                    aux_info = aux_info, 
                                                    variable = variable, 
                                                    max_dist = max_dist, 
                                                    window_width = window_width, 
                                                    min_coverage = min_coverage, 
                                                    min_correlation = min_correlation,
                                                    min_station = min_station, 
                                                    max_station = max_station, 
                                                    max_res = max_res, 
                                                    max_res_norm = max_res_norm)
  } else if(variable == 'Precip'){
    variable_consistency <- NA
    
    spatial_consistency <- test_precipitation_spatial_corrobation(weather = weather, 
                                                                  weather_coords = weather_coords,
                                                                  aux_info = aux_info,
                                                                  aux_list = aux_list,
                                                                  max_dist = max_dist,
                                                                  max_station = max_station,
                                                                  min_station = min_station)
  }
  
  
  
  test_res <- tibble('fixed_limit' = fixed_lim, 
                     'variable_limit' = variable_lim, 
                     'temporal_consistent' =  temporal_consistency, 
                     'consistent_variables' = variable_consistency, 
                     'spatial_consistent' = spatial_consistency)
  
  test_res$outlier <- rowSums(test_res, na.rm = T) >= 2
  
  return(test_res)
}