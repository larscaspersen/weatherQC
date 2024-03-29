#' Test for exceedence of climatological limits
#' 
#' Checks if daily weather observations or outside the usual climatological ranges of that weather station.
#' 
#' The function calculates monthly and annual percentiles of Tmin and Tmax and 
#' then tests if the observation exceed the upper and lower percentile.
#' This function is part of the weather quality control tests of \insertCite{costa_gap_2021;textual}{weatherQC} and is contained by the function 
#' \code{costa_qc()}
#'
#' @param weather 	a data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable in weather
#' @param probs a numeric vector of length two, containing the percentiles used 
#' to calculate the test threshold. Default is \code{c(0.01, 0.99)}
#' @return Logical vector of same length as rows in \code{weather}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test.
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' test_variable_limit(weather = target_weather, variable = "Tmin")
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_variable_limit <- function(weather, variable, probs = c(0.01, 0.99)){
  
  #avoid note by cmd test
  . <- NULL
  
  #split to monthly groups
  monthly_weather <- split(weather, f = weather$Month)
  
  
  monthly_test <- weather %>%
    split(f = .$Month) %>%
    purrr::map(function(x) stats::quantile(x[,variable], probs = probs, na.rm = T)) %>%
    purrr::map2(monthly_weather, function(x,y) ifelse(is.na(y[,variable]), yes = FALSE, no = y[,variable] < x[1] | y[,variable] > x[2])) %>%
    unsplit(f = weather$Month)
  
  
  #also test for yearly quantiles
  test_quantiles <- stats::quantile(weather[,variable], probs = c(probs[1], probs[2]), na.rm = T)
  
  yearly_test <- ifelse(is.na(weather[,variable]), yes = F, 
         no = ((weather[, variable] < test_quantiles[1])|(weather[,variable]>test_quantiles[2])))
  
  
  return(yearly_test | monthly_test)
  
}