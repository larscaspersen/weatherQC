#' Identify instances of false zeros in weather data
#' 
#' The function tries to identify instances of false zeros in temperature
#' data.
#' 
#' The function tests for instances in which both minimum and maximum temperature
#' received values of 0 degree C or -17.8 degree C (= 0 degree F) but the authors 
#' more likely intended to record missing data instead. Both variables need to
#' have 0 or -17.8 as a value for the test to return a flag.
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 3 "Basic integrity tests".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples test_naught_weather(weather = weather)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
test_naught_weather <- function(weather){
  flag <- ifelse((weather$Tmin == -17.8 & weather$Tmax == -17.8) | (weather$Tmin == 0 & weather$Tmax == 0),
                 yes = TRUE, no = FALSE)
  
  #change nas to false
  flag[is.na(flag) == TRUE] <- FALSE
  return(flag)
}