#' Test lagged temperature range
#' 
#' The function performs a set of 6 tests regarding the temperature range over
#' a range of three days per observation.
#' 
#' The test checks if the range of Tmin, Tmax and Tmean over a period of three
#' days, centered at the day of interest, is larger than a default threshold
#' of 40 degree C. If so, all values involved in the positive test are flagged.
#' 
#' The set of test include minimum temperature (Tmin), maximum temperature (Tmax) and
#' mean temperature (Tmean) of the current day (0), previous day (-1) and following
#' day (1). The tests are as followed:
#' \itemize{
#'  \item{Test 1: Tmax(0) >= max[Tmin(-1:1)] + 40}
#'  \item{Test 1: Tmax(0) >= max[Tmean(-1:1)] + 40}
#'  \item{Test 1: Tmin(0) <= max[Tmax(-1:1)] - 40}
#'  \item{Test 1: Tmin(0) <= max[Tmean(-1:1)] - 40}
#'  \item{Test 1: Tmean(0) <= max[Tmax(-1:1)] - 40}
#'  \item{Test 1: Tmean(0) >= max[Tmin(-1:1)] + 40}
#' }
#' 
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 5 "Internal and temporal consistency checks".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable 
#' in weather
#' @param max_diff threshold for the test, spike and dips lower than this value
#' are ignored
#' @return data.frame with two columns and same amount of rows as in \code{weather}. 
#' One column is for the flag of minimum temperature, the other for maximum temperature.
#' Values of \code{TRUE} indicate successful test, meaning that the tested 
#' variable exceeded the limits of the test.
#' @examples test_lagged_temperature_range(weather = weather)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_lagged_temperature_range <- function(weather, max_diff = 40){
  #get lowesr tmax for each day using a trhee day window
  lowest_tmax <- apply(matrix(c(lag(weather$Tmax), weather$Tmax, lead(weather$Tmax)),nrow = nrow(weather),
                              ncol = 3, byrow = FALSE), MARGIN = 1, function(x){
                                if(all(is.na(x))){
                                  return(NA)
                                } else(
                                  min(x, na.rm = T)
                                )
                              } )
  
  #create flag for tmin and tmax. each day of tmin tested true gets flagged, aswell as the tree day windows of tmax
  tmin_flag <- tmax_flag <- weather$Tmin <= lowest_tmax - max_diff
  addtional_true <- c(which(tmax_flag) + 1, which(tmax_flag) - 1) %>%
    .[. != 0 | .!= length(tmin_flag)]
  tmax_flag[addtional_true] <- TRUE
  
  #same for tmax
  #get lowesr tmax for each day using a trhee day window
  highest_tmin <- apply(matrix(c(lag(weather$Tmin), weather$Tmin, lead(weather$Tmin)),nrow = nrow(weather),
                               ncol = 3, byrow = FALSE), MARGIN = 1,  function(x){
                                 if(all(is.na(x))){
                                   return(NA)
                                 } else(
                                   max(x, na.rm = T)
                                 )
                               })
  
  #create flag for tmin and tmax. each day of tmin tested true gets flagged, aswell as the tree day windows of tmax
  tmin_flag2 <- tmax_flag2 <- weather$Tmax >= highest_tmin + max_diff
  addtional_true <- c(which(tmax_flag2) + 1, which(tmax_flag2) - 1) %>%
    .[. != 0 | .!= length(tmin_flag)]
  tmax_flag2[addtional_true] <- TRUE
  
  #remove nas from flag, change them to NA
  tmin_flag <- replace_na(tmin_flag | tmin_flag2, replace = FALSE)
  tmax_flag <- replace_na(tmax_flag | tmax_flag2, replace = FALSE)
  
  
  #return any case of tmin_flag / tmin_flag2; tmax_flag | tmax_flag2
  return(data.frame(tmin_flag, tmax_flag))
}