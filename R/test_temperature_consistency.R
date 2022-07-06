#' Check if Tmin, Tmax and Tmean are in line
#' 
#' This function does basic integrity test on minimum, maximum and average daily temperature.
#' 
#' The test composes of four sub-tests, if any of the tests yield positive outcome, 
#' then the temperature data of that day are flagged as suspicious.
#' 
#' @details
#' \itemize{
#'  \item{Test 1: Minimum temperature of day 0 should not be higher than maximum temperature of days -1, 0 and 1}
#'  \item{Test 2: Minimum temperature of day 0 should not be higher than mean temperature of day -1, 0, or 1}
#'  \item{Test 3: Maximum temperature of day 0 should not be higher than mean temperature of day -1, 0, or 1}
#'  \item{Test 4: The absolute difference of reported mean temperature and computed 
#'  mean temperature (Tmin + Tmax)/2 exceeding the 99\% percentile of all observed values.}
#' }
#' 
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param probs numerical indicating the percentile to calculate the test limit
#' @return logical vector of same length as rows in \code{weather}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test.
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' test_temperature_consistency(weather = KA_weather)
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_temperature_consistency <- function(weather, probs = 0.99){
  #minimum temperature should not be higher than maximum temperature (also one day earlier or later)
  cons1 <- (weather$Tmin >= weather$Tmax) | (weather$Tmin >= dplyr::lag(weather$Tmax)) | (weather$Tmin >= dplyr::lead(weather$Tmax))
  
  #min temperature should be lower than tmean
  cons2 <- (weather$Tmin >= weather$Tmean) | (weather$Tmin >= dplyr::lag(weather$Tmean)) | (weather$Tmin >= dplyr::lead(weather$Tmean))
  
  #maximum temperature should be always higher than mean temperature
  cons3 <- (weather$Tmax <= weather$Tmean) | (weather$Tmax <= dplyr::lag(weather$Tmean)) | (weather$Tmax <= dplyr::lead(weather$Tmean))
  
  #tmean should not be too different from (tmin + tmax)/2
  diff_tmean <- abs(((weather$Tmin + weather$Tmax)/2) - weather$Tmean)
  perc_diff_tmean <- quantile(diff_tmean, prob = probs, na.rm = T)
  cons4 <- diff_tmean > perc_diff_tmean
  
  
  return(ifelse(test = is.na(cons1 | cons2 | cons3 | cons4), yes = F, no = (cons1 | cons2 | cons3 | cons4)))
  
}