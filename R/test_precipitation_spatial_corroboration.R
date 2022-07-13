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
#' Climatological percentile ranks of precipitation are calculated using the following approach.
#' It includes precipitation data of a 29 day window centered at
#' the day of interest throughout all observed years and calculates the empirical
#' cumulative density function (ecdf). Missing observations and 
#' zer-precipitation observations are discarded. If there are fewer observations than 
#' 20 days, then NA is returned instead of the ecdf. Then the 
#' precipitation event of interest is inserted in the ecdf and the corresponding 
#' percentile rank is calculated. This is done for each precipitation event.
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
#' included in the spatial corroboration test
#' @param max_station maximum number of neighbouring stations included in the test.
#' If more auxiliary stations available than \code{max_station}, then closest ones
#' are taken
#' @param min_station minimum amount of neighbouring stations for the test. If less
#' is available, then test is not carried out and automatically returns \code{FALSE}
#' for every observation
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples test_precipitation_spatial_corroboration(weather = target_weather,
#' weather_coords = c(target_info$Longitude, target_info$Latidue),
#' aux_info = neighbour_info, aux_list = neighbour_weather)
#' @seealso \code{\link{test_temperature_corroboration}}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_precipitation_spatial_corroboration <- function(weather, weather_coords, aux_info, 
                                                   aux_list, max_dist = 75,
                                                   max_station = 7, min_station = 3){
  
  #calculate distance to aux_stations
  aux_info$dist <-  round(sp::spDistsN1(pts = as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                        pt = weather_coords, longlat = TRUE), 2)
  
  #select stations within the max distance, which are not the target station
  aux_info <- aux_info %>%
    dplyr::filter(.data$dist > 0 & .data$dist <= max_dist) %>%
    dplyr::arrange(.data$dist)
  
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
  aux_list <-  purrr::map(aux_list, get_prec_rank) %>%
    purrr::map2(.data, aux_list, function(x,y) tibble::tibble(y, prec_rank = x))
  
  ####
  #get absolute minimum difference
  ####
  
  #create a matrix with lagged, normal and lead observartion of variable in aux_list
  x_org <- data.frame(weather[,c('Date', 'Precip')])
  names(x_org)[2] <- 'x'
  
  #extract the precipitation data from the aux list
  y_org <- purrr::map(aux_list, function(y){
    int <- merge.data.frame(x_org, y[,c('Date', 'Precip')], 
                            by = 'Date', all.x = T)
    return(int[,'Precip'])}) %>%
    dplyr::bind_cols()
  
  #bind by columns to a matrix
  y_org <- tibble::tibble(y_org, dplyr::lead(y_org), dplyr::lag(y_org), .name_repair = 'minimal') %>%
    as.matrix() 
  
  #if x isnt the larger/smaller than all y in a row, return zero, 
  #otherwise return minimum absolute difference

  y_org_min_abs <- apply(cbind(x_org[2],y_org),MARGIN = 1, function(x){
    i <- length(x)
    x<-  unname(x)
    #case that x is na
    if(is.na(x[1]) == T){
      return(NA)
    #case that y only contains NAs
    } else if(all(is.na(x[2:i])) == T){
      return(NA)
    #regular case
    } else if(x[1] > max(x[2:i], na.rm = T) | x[1] < min(x[2:i], na.rm = T)){
      
      return(min(abs(x[1] - x[2:i]), na.rm = T))
    #case that x is not the largest or the smallest
    } else{
      return(0)
    }
    
  })
  
  #repeat the same thing for precipitation rank
  x_rank <- data.frame(weather[,c('Date', 'prec_rank')])
  names(x_rank)[2] <- 'x'
  
  #extract the precipitation data from the aux list
  y_rank <- purrr::map(aux_list, function(y){
    int <- merge.data.frame(x_rank, y[,c('Date', 'prec_rank')], 
                            by = 'Date', all.x = T)
    return(int[,'prec_rank'])}) %>%
    dplyr::bind_cols()
  
  #bind by columns to a matrix
  y_rank <- tibble::tibble(y_rank, dplyr::lead(y_rank), dplyr::lag(y_rank), .name_repair = 'minimal') %>%
    as.matrix() 
  
  y_rank_min_abs <- apply(cbind(x_rank[2],y_rank),MARGIN = 1, function(x){
    i <- length(x)
    x<-  unname(x)
    #case that x is na
    if(is.na(x[1]) == T){
      return(NA)
      #case that y only contains NAs
    } else if(all(is.na(x[2:i])) == T){
      return(NA)
      #regular case
    } else{
      
      return(min(abs(x[1] - x[2:i]), na.rm = T))
      #case that x is not the largest or the smallest
    } 
    
  })
  
  #determine testing threshold
  
  test_threshold <- ifelse(is.na(y_rank_min_abs), yes = 269.24, no = -45.72 * log(y_rank_min_abs) + 269.24)
 
  flag <- ifelse(is.na(y_org_min_abs), yes = FALSE, no = y_org_min_abs > test_threshold)
  
 
  return(flag)
  
}