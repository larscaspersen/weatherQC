#' Test the spatial consistency of target station for one peroid
#' 
#' Function checks if target temperature data is in line of neighboring 
#' weather stations by linear regression.
#' 
#' The function is a helper function for the higher-level function \code{\link{test_spatial_consistency}}
#' The function takes temperature data of the target and auxiliary weather station for a
#' certain month of a certain year plus further observations prior and after the month defined
#' by the parameter \code{window_width}. By default, target - auxiliary weather stations need to have 
#' at least \code{min_coverage = 40} shared observations to be considered for linear 
#' regression. In the next step the index of agreement between target and auxiliary weather
#' stations is calculated. For each value in the target station the closest value of the
#' each neighboring series is retrieved using a three day window centered at the
#' day of interest. The so obtained time series of each neighbor station, called
#' closest_y, is then used for the linear regressions. Weather stations, which 
#' model linear regression output yielded a correlation score of 0.8 or lower
#' with actual observations in the target station are dropped. Furthermore,
#' if more than \code{max_station} auxiliary weather stations are available,
#' then the ones with the lowest index of agreement are dropped. Weighted mean
#' of regression output is calculated using the index of agreement as weights. Regular 
#' and standardized (by subtracting mean residual and dividing by standard
#' deviation of residuals) model residuals are calculated. Model results
#' within the extra window period are dropped. Remaining residuals are checked
#' if they are larger than thresholds defined in \code{max_res} and 
#' \code{max_res_norm}. Observations for which the regular residual and the 
#' standardized residual both exceed their respective thresholds are flagged.
#' 
#' For more information please refer to \insertCite{durre_comprehensive_2010;textual}{weatherQC}
#' section 6 "Spatial consistency checks" and "Appendix B".
#' 
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param aux_info data.frame listing the auxiliary weather stations. Should at least contain
#' the columns c("id", "Longitude", "Latitude")
#' @param aux_list named list of data.frames with daily weather obsrvations of auxiliary
#' weather stations. Names should be identical to \code{aux_info$id}. Strucuture of 
#' data.frames should be identical of \code{weather}. Data.frames do not necissarily
#' need to cover excat same time period as \code{weather}
#' @param period_start Date, indicating the start of the month
#' @param variable column in \code{weather} for which the test is performed. Should
#' be either Tmin or Tmax. data.frames in \code{aux_list} need to have the same
#' name
#' @param max_res testing threshold, highest regular resiudal tolerated by test
#' @param max_res_norm testing threshold, highest standardized residual tolerated
#' by the test. Note: both thresholds need to be exceeded in order for the 
#' test to yield a flag
#' @param max_station maximum number of neighbouring stations included in the test.
#' If more auxiliary stations available than \code{max_station}, then closest ones
#' are taken
#' @param min_station minimum amount of neighbouring stations for the test. If less
#' is available, then test is not carried out and automatically returns \code{FALSE}
#' for every observation
#' @param window_width amount of extra days added to the target and auxiliary
#' weather station for the linear regression. Extra days only part of the model
#' construction, not the testing
#' @param min_correlation minimum correlation of model output x target station for 
#' an auxiliary weather station to be considered in the test
#' @param min_coverage minimum amount of shared observations of target and
#' auxiliary weather stations, so that auxiliary weather station is considered
#' for the test
#' @return logical vector of same length as target month has days. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples spat_consist_one_period(weather = target_weather,
#' aux_info = neighbour_info, aux_list = neighbour_weather, 
#' period_start = as.Date("1992-01-01", format = "%Y-%m-%d"), variable = "Tmin")
#' @seealso \code{\link{get_abs_min_difference}}, \code{\link{get_closest_y}}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
spat_consist_one_period <- function(weather, aux_list, aux_info, period_start, variable,
                                    max_res = 8, max_res_norm = 4, min_station = 3,
                                    max_station = 7, window_width = 15, 
                                    min_correlation = 0.8, min_coverage = 40){
  
  
  #add window width to the period
  period_end <- lubridate::ceiling_date(period_start,unit = 'month') + window_width -1
  period_start <- period_start - window_width
  
  #extract data from target (x) and aux (y)
  x <- select_target_days(df = weather, variable = variable, period_start = period_start, period_end = period_end)
  y <- purrr::map(aux_list, function(x){
    select_target_days(df = x, variable = variable, period_start = period_start, 
                       period_end = period_end)
  }) %>%
    do.call(cbind.data.frame, .data)
  
  #only keep aux stations which fulfill coverage criteria
  aux_info <- aux_info[colSums(is.na(x) == F & is.na(y) == F) >= min_coverage, ]
  
  #drop stations from y
  y <- y[,aux_info$id]
  
  
  #case there are not enough stations left: return nas as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  }
  
  #calcualte index of agreement and sort decreasing
  aux_info$ind_agreement <- purrr::map_dbl(y, ~ calc_index_agreement(x = x, y = .x))
  aux_info <- aux_info[order(aux_info$ind_agreement,decreasing = T),]
  
  #bring y in same order
  y <- y[,aux_info$id]
  
  #iterate over all y columns, for each column iterate over x and find the closest y value given a 3 day window centered around i
  y_closest <-  purrr::map(y, function(vec) purrr::imap_dbl(x,~get_closest_y(x = .x, y=vec, i = .y))) %>%
    do.call(cbind.data.frame, .)
  
  #carry out linear regression
  models <- purrr::map(y_closest, ~ lm(x~.x))
  
  
  #calculate correlation coefficient, filter stations with too low correlation coefficient
  #keep only stations fulfillinf the criteria of minimum correlation of their prediction
  aux_info <- purrr::map_lgl(models, ~sqrt(summary(.x)[['r.squared']]) > min_correlation) %>%
    aux_info[.,]
  
  
  #if there are less then 3 stations remaining, then return NAs as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  } else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  }
  
  models <-   models[aux_info$id]
  y_closest <- y_closest[,aux_info$id]
  
  helper_func <- function(x,y,z){
    z -as.numeric((x %*% as.matrix(y)) / sum(y))
  }
  
  #calculate weighted model estimates for each day
  #problem: nas are not returned here, I need to match it with x
  x_res <- purrr::map2(models, y_closest, .f = function(x,y){
    x$coefficients[1] +  x$coefficients[2] * y}) %>%
    dplyr::bind_cols() %>%
    as.matrix() %>%
    helper_func(aux_info$ind_agreement, x) %>%
    round(digits = 2)
  
  rm(models)  
  
  #standardized residuals (by mean and std)
  x_res_norm <- (x_res - mean(x_res, na.rm = T)) / stats::sd(x_res, na.rm = T)
  
  
  #take only the values for the month
  #--> strip the leading and trailing 15 values
  x_res <- x_res[(window_width+1):(length(x_res) - (window_width))]
  x_res_norm <- x_res_norm[(window_width+1):(length(x_res_norm) - (window_width))]
  
  flag_res <- ifelse(is.na(x_res), yes = F, no = abs(x_res) >= max_res)
  flag_res_norm <-  ifelse(is.na(x_res_norm), yes = F, no = abs(x_res_norm) >= max_res_norm)
  
  flag <- tidyr::replace_na(flag_res & flag_res_norm, FALSE)
  
  #if either the residuals or the standardized resiudals exceed the threshold, return for that given day a true
  return(flag)
  
}