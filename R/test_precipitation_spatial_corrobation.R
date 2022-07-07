#' Spatial corroboration test of precipitation
#' 
#' Function to test if the target precipitation is spatial coherent with neighbor
#' observations
#' 
#' The function compares the precipitation observation of the target station to the
#' observations of neighboring stations. Test limit depends on the absolute difference
#' in climatological percentile ranks of precipitation observation. For a detailed
#' description of the test please refer to \insertCite{durre_comprehensive_2010;textual}{weatherQC}
#' section 6 "Spatial consistency checks" and "Appendix C". 
#' 
#' Climatological percentile ranks of precipitation is calculated using \code{\link{get_prec_rank}}
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
#' @param max_dist maximum distance in kilometers of neighbouring stations to target station to be 
#' included in the spatial corrobation test
#' @param max_station maximum number of neighbouring stations included in the test.
#' If more auxiliary stations available than \code{max_station}, then closest ones
#' are taken
#' @param min_station minimum amount of neighbouring stations for the test. If less
#' is available, then test is not carried out and automatically returns \code{FALSE}
#' for every observation
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples test_precipitation_spatial_corrobation(weather = target_weather,
#' weather_coords = c(target_info$Longitude, target_info$Latidue),
#' aux_info = neighbour_info, aux_list = neighbour_weather)
#' @seealso \code{\link{get_prec_rank}}, \code{\link{get_abs_min_difference}}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_precipitation_spatial_corrobation <- function(weather, weather_coords, aux_info, 
                                                   aux_list, max_dist = 75,
                                                   max_station = 7, min_station = 3){
  
  #calculate distance to aux_stations
  aux_info$dist <-  round(sp::spDistsN1(pts = as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                        pt = weather_coords, longlat = TRUE), 2)
  
  #select stations within the max distance, which are not the target station
  aux_info <- aux_info %>%
    filter(dist > 0 & dist <= max_dist) %>%
    arrange(dist)
  
  #if too few neighbouring values, then the test can't be carried out
  if(nrow(aux_info) < min_station){
    return(rep(FALSE, nrow(weather)))
  }  else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  } 
  aux_list <- aux_list[aux_info$id]
  
  ####
  #calculate prec rank
  ####
  
  #calculate for each precipitation value the percentile rank
  weather$prec_rank <- get_prec_rank(weather = weather)
  
  #also add prec rank to aux data
  aux_list <-  map(aux_list, get_prec_rank) %>%
    map2(., aux_list, function(x,y) tibble(y, prec_rank = x))
  
  ####
  #get absolute minimum difference
  ####
  
  #now determine the prec rank difference  
  prec_min_difference <- map2_dbl(weather$Precip, weather$Date, function(x,y) get_abs_min_difference(x = x, target_date = y, variable = 'Precip', aux_list = aux_list))
  #same for precipitation percentile rank
  prec_rank_difference <-  map2_dbl(weather$prec_rank, weather$Date, function(x,y) get_abs_min_difference(x = x, target_date = y, variable = 'prec_rank', aux_list = aux_list))
  
  
  ###
  #get test threshold & and test
  ###
  test_threshold <- (-45.72 * log(prec_rank_difference) + 269.24)
  
  return(replace_na(prec_min_difference > test_threshold, replace = FALSE))
  
}