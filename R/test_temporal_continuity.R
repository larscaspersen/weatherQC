#' Test for sudden spikes or dips
#' 
#' The function checks for sudden day-to-day jumps or drops in daily weather data.
#' 
#' The decision criterion when strong day-to-day changes in the weather variable 
#' become suspicious is for this test based on the 99.5\% percentile of all day-to-day
#' changes in the variable. Every absolute day-to-day change above the percentile
#' is flagged as suspicious. The function is part of the Costa et al. (2021) 
#' \insertCite{costa_gap_2021}{weatherQC} quality control algorithm which can be
#' called via \code{costa_qc()}. 
#'
#' @param weather 	a data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable in weather
#' @param prob a numerical indicating the percentile which is used as a threshold for suspicious
#' observations
#' @return Logical vector of same length as rows in \code{weather}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test.
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' test_temporal_continuity(weather = KA_weather, variable = "Tmin")
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
test_temporal_continuity <-  function(weather, variable, prob = 0.995){
  #calculate difference to next day, append one NA to keep same length
  diffs <- c(abs(diff(as.matrix(weather[,variable]))), NA)
  
  #get the quantile of the jumps, everything above the quantile is labelled as suspicious
  jump_quan <- quantile(diffs, probs = prob, na.rm = T)
  
  return(ifelse(is.na(diffs), yes = F, no = (diffs > jump_quan)))
  
}