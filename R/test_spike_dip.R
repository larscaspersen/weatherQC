#' Test for spike followed by a dip in temperature data
#' 
#' The function searches for strong spikes directly followed by dips in daily
#' temperature data.
#' 
#' The concept of the function is similar to \code{\link{test_temporal_continuity}},
#' but there are some important differences. This function requires a spike immediately
#' followed by a dip (or vice versa), while \code{test_temporal_continuity} or 
#' requires one of the two for a positive test result. Furthermore, \code{test_spike_dip}
#' works with hard thresholds, while \code{test_temporal_continuity} works with 
#' thresholds defined by percentiles of the weather data. In general \code{test_spike_dip}
#' is a much more restrictive test than \code{test_temporal_continuity}.
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 5 "Internal and temporal consistency checks".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable 
#' in weather
#' @param dip_threshold threshold for the test, spike and dips lower than this value
#' are ignored
#' @return Logical vector of same length as rows in \code{weather}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test.
#' @examples test_spike_dip(weather = target_weather, variable = "Tmin")
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_spike_dip <- function(weather, variable, dip_threshold = 25){
  flag <- abs(weather[,variable] - lead(weather[,variable])) >= dip_threshold & abs(weather[,variable] - lag(weather[,variable])) >= dip_threshold
  
  flag <- replace_na(flag[,1], FALSE)
  return(flag)
}