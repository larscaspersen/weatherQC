#' Test the spatial consistency of target station
#' 
#' Function checks if target temperature data is in line of neighbouring 
#' weather stations by linear regression.
#' 
#' The function takes temperature data of the target and auxiliary weather station for a
#' certain month of a certain year plus further observations prior and after the month defined
#' by the parameter \code{window_width}. By default, target - auxiliary weather stations need to have 
#' at least \code{min_coverage = 40} shared observations to be considered for linear 
#' regression. In the next step the index of agreement betwen target and auxiliar weather
#' stations is calculated. For each value in the target station the closest value of the
#' each neighbouring series is retrieved using a three day window centered at the
#' day of interest. The so obtained timeseries of each neighbour station, called
#' closest_y, is then used for the linear regressions. Weather stations, which 
#' model linear regression output yielded a correlation score of 0.8 or lower
#' with actual observations in the target station are dropped. Furthermore,
#' if more than \code{max_station} auxiliary weather stations are available,
#' then the ones with the lowest index of agreement are dropped. Weighted mean
#' of regression output is calculted using the index of agreement as weights. Regular 
#' and standardized (by subtracting mean residual and deviding by standard
#' deviation of resiudlas) model residuals are calculated. Model results
#' within the extrat window period are dropped. Remainin residuals are checked
#' if they are larger than thresholds defined in \code{max_res} and 
#' \code{max_res_norm}. Observations for which the regular residual and the 
#' standardized resiudual both exceed their respective threhsolds are flagged.
#' 
#' For more information please refer to Durre et al. (2010) \insertCite{durre_comprehensive_2010}{weatherQC}
#' section 6 "Spatial consistency checks" and "Appendix B".
#' 
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param weather_coords numerical vector of length two. Should contain longitude and 
#' latitude (in that order) of target station in decimal format
#' @param aux_info data.frame listing the auxiliary weather stations. Should at least contain
#' the columns c("id", "Longitude", "Latitude")
#' @param aux_list named list of data.frames with daily weather obsrvations of auxiliary
#' weather stations. Names should be identical to \code{aux_info$id}. Strucuture of 
#' data.frames should be identical of \code{weather}. Data.frames do not necissarily
#' need to cover excat same time period as \code{weather}
#' @param variable column in \code{weather} for which the test is performed. Should
#' be either Tmin or Tmax. data.frames in \code{aux_list} need to have the same
#' name
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
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples test_spatial_consistency(weather = weather,
#' weather_coords = c(weather_info$Longitude, weather_info$Latidue),
#' aux_info = aux_info, aux_list = aux_list, variable = "Tmin")
#' @seealso \code{\link{spat_consist_one_period}}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
test_spatial_consistency <- function(weather, weather_coords, aux_list, aux_info, 
                                     variable, max_dist = 75, window_width = 15, 
                                     min_coverage = 40, min_correlation = 0.8,
                                     min_station = 3, max_station = 7, max_res = 8, 
                                     max_res_norm = 4){
  
  
  #calculate distance to aux_stations
  aux_info$dist <-  round(sp::spDistsN1(pts = as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                        pt = weather_coords, longlat = TRUE), 2)
  
  #select stations within the max distance, which are not the target station
  aux_info <- aux_info %>%
    filter(dist > 0 & dist <= max_dist)
  
  aux_list <- aux_list[aux_info$id]
  
  
  
  ###get vector of start and end dates for extraction period
  
  #get first and last month, create sequence of starting months, add extra days by window width
  first_month <- lubridate::floor_date(weather$Date[1], unit = 'month')
  last_month <- lubridate::floor_date(weather$Date[nrow(weather)], unit = 'month')
  starts <- seq(from = first_month, to = last_month, by = 'months')
  
  
  #calculate flags for each month
  spatial_flags <- purrr::map(starts, function(x){
    spat_consist_one_period(weather = weather, aux_list = aux_list,
                            aux_info = aux_info, variable = variable, period_start = x, 
                            window_width = window_width, max_res = max_res, 
                            max_res_norm = max_res_norm, min_station = min_station, 
                            max_station = max_station, min_correlation = min_correlation, 
                            min_coverage = min_coverage)})
  
  
  #chnage nas to false
  spatial_flags <- replace_na(unlist(spatial_flags), FALSE)
  
  #return the the flags in form of a list
  return(spatial_flags)
  
}  