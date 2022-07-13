#' Spatial corroboration test of temperature
#' 
#' Function to test if the target temperature is spatial coherent with neighbour
#' observations.
#' 
#' The function compares the temperature observation of the target station to the
#' observations of neighboring stations. This function is a companion test to
#' \code{\link{test_spatial_consistency}}. While the aim is similar (identify
#' observations which are not in line with neighboring observations) the testing 
#' principle is different. While \code{test_spatial_consistency} compares the weighted
#' output of linear regression to the target value, the \code{test_temperature_corroboration}
#' compares the target observation to the temperature anomaly of the station to the 
#' long term mean. The testing tolerance of \code{test_temperature_corroboration} is wider, so it is
#' less strict, but it also makes fewer demands on the shared observation of 
#' target and auxiliary weather stations, so it can be applicable where the demands
#' of \code{test_spatial_consistency} do not permit any test result at all. 
#' 
#' The testing procedure is a follows: The target observation is compared with at
#' least 3 but never more than 7 neighborliness stations within a 75 km radius.
#' Neighboring values of the target variable of the same day, but also the previous and
#' following day are included in the test to account for differences in the recording
#' protocol. For each observation and station the temperature anomaly is calculated,
#' meaning the standardized residuals of observation to the long term mean of the day of the year
#' of the respective weather station. Long term mean and standard deviation are 
#' calculated using the same principle as in \code{\link{get_longterm_mean_and_sd}}.
#' If the minimum absolute difference of the target to neighbor anomaly is larger than 
#' the default threshold of 10 degree C, then the target value is flagged by the 
#' test.
#' 
#' For a detailed description of the test please refer to \insertCite{durre_comprehensive_2010;textual}{weatherQC}
#' section 6 "Spatial consistency checks". 
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
#' @param variable a character indicating the column name of the tested variable 
#' in weathe
#' @param max_dist maximum distance in kilometers of neighbouring stations to target station to be 
#' included in the spatial corroboration test
#' @param max_station maximum number of neighbouring stations included in the test.
#' If more auxiliary stations available than \code{max_station}, then closest ones
#' are taken
#' @param min_station minimum amount of neighbouring stations for the test. If less
#' is available, then test is not carried out and automatically returns \code{FALSE}
#' for every observation
#' @param max_diff maximum difference of the lowest minimum difference of target observation
#' to neighbouring observations for temperature anomalies
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples test_temperature_corroboration(weather = target_weather,
#' weather_coords = c(target_info$Longitude, target_info$Latidue),
#' aux_info = neighbour_info, aux_list = neighbour_list, variable = "Tmin")
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_temperature_corroboration <- function(weather, weather_coords,
                                         aux_list, aux_info,
                                         variable,
                                         max_station = 7, min_station = 3,
                                         max_dist = 75, max_diff = 10){
  #get climate mean and sd for each day of target station
  #and add it to weather data frame
  weather <- purrr::map(unique(weather$doy), ~get_longterm_mean_and_sd(weather = weather, variable = variable,
                                                                doy = .x)) %>%
    dplyr::bind_rows() %>%
    merge(weather, by = 'doy', all.y = TRUE) %>%
    dplyr::arrange(.data$Date)
  
  #calculate climate anomaly
  weather$anomaly <- weather[,variable] - weather$mean
  
  #calculate temperature anomalies for a subset of closest stations of a three day window
  #take anomaly closest to target anomaly
  #if difference is greater than 10°C, then target value is flagged
  
  #this should be done ONCE, it happens also in the temperature spatial consistency test
  
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
  
  aux_list <- purrr::map(aux_list, function(x){
    
    #add date and doy to aux data
    x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d")
    x$doy <- lubridate::yday(x$Date)
    #calculate climate mean and sd of aux data
    climate_df <- purrr::map(unique(x$doy), ~get_longterm_mean_and_sd(weather = x, variable = variable,
                                                               doy = .x)) %>%
      dplyr::bind_rows() %>%
      merge(x, by = 'doy', all.y = TRUE) %>%
      dplyr::arrange(.data$Date)

    #calculate climate anomaly of aux data
    x$anomaly <- (x[[variable]] - climate_df$mean)
    
    return(x)
  }) 
  
  #create a matrix with lagged, normal and lead observartion of variable in aux_list
  x <- data.frame(weather[,c('Date', 'anomaly')])
  names(x)[2] <- 'x'
  
  #extract data from aux_list
  y <- purrr::map(aux_list, function(y){
      int <- merge.data.frame(x, y[,c('Date', 'anomaly')], 
                       by = 'Date', all.x = T)
      return(int[,'anomaly'])}) %>%
    dplyr::bind_cols()
  
  #bind by columns to a matrix
  y <- tibble::tibble(y, dplyr::lead(y), dplyr::lag(y), .name_repair = 'minimal') %>%
    as.matrix() 
  
  #find minimum absolute difference, check if larger than threshold
  #cases in that min() only contain NA returns Inf, change Inf to NA instead
  flag <- abs(x$x - y) %>%
    apply(MARGIN = 1, function(x){
      #check if it only contains NAs
      if(all(is.na(x))){
        return(NA)
      } else{
        min(x, na.rm = T)
      }
    })  >= max_diff
  
  return(ifelse(is.na(flag), yes = FALSE, no = flag))
}