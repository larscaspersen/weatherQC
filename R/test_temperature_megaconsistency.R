#' Identify temperature inconsistency within calendar months
#' 
#' The function checks if minimum temperature exceeds the highest maximum temperature
#' of a calendar month or vice versa.
#' 
#' The test is similar to the test regarding internal consistency of temperature.
#' In the regular consistency test, minimum or maximum observations which did not 
#' have the accompanying other extreme temperature observation may have slipped
#' the regular consistency test. The megaconsistency test does not require for
#' a temperature observation to have the other "partnering" temperature observation 
#' and thus fill the gap.
#' 
#' This function is part of the weather quality control scheme after Durre et al.
#' (2010) \insertCite{durre_comprehensive_2010}{weatherQC}. For more details 
#' please refer to section 7 "Megaconsistency tests".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param min_obs minimum amount of observation per calendar month in order 
#' to be considered in the test, otherwise for the calendar month all observations
#' automatically get flagged as FALSE
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples test_temperature_megaconsistency(weather = target_weather)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_temperature_megaconsistency <- function(weather, min_obs = 140){
  
  #for each month get max(tmax) and min(tmin)
  flags <- weather %>%
    dplyr::group_by(.data$Month) %>%
    dplyr::mutate('flag_tmin' = .data$Tmin > max(.data$Tmax, na.rm = T),
           'flag_tmax' = .data$Tmax < min(.data$Tmin, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$Month, .data$flag_tmin, .data$flag_tmax) %>%
    dplyr::mutate('flag_tmin' = tidyr::replace_na(.data$flag_tmin, FALSE),
           'flag_tmax' = tidyr::replace_na(.data$flag_tmax, FALSE))
  
  #check if each month has enough observations, otherwise replace flags with FALSE
  obs_tmax <- weather %>%
    dplyr::group_by(.data$Month) %>%
    dplyr::select(.data$Tmax) %>%
    stats::na.omit() %>%
    dplyr::summarise('n' = dplyr::n())
  
  obs_tmin <- weather %>%
    dplyr::group_by(.data$Month) %>%
    dplyr::select(.data$Tmin) %>%
    stats::na.omit() %>%
    dplyr::summarise('n' = dplyr::n())
  
  too_few_obs <- which((obs_tmin$n >= min_obs) & (obs_tmax$n >= min_obs) == FALSE)
  
  #in case there are months with too few observations, replace Flag with FALSE
  if(purrr::is_empty(too_few_obs) == FALSE){
    
    flags[flags$Month == too_few_obs, c('flag_tmin', 'flag_tmax')] <- FALSE
  }
  
  return(flags[, c('flag_tmin', 'flag_tmax')])
}