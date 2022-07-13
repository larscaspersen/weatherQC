#' Check for temporal consistency of temperature variables
#' 
#' The function carries out a set of test concerning the temporal consistency of
#' minimum and maximum temperature. For better testing performance it is adviced
#' to also supply the average temperature. 
#' 
#' The testing algorithm carries out a set of 7 consistency test for each day of the
#' year. Observations with the highest amount of positive test results are flagged and
#' the test is carried out until no more new flags are added.
#' 
#' The consistency tests of minimum temperature (Tmin), maximum temperature (Tmax) and
#' mean temperature (Tmean) of the current day (0) and following day(1) are as followed:
#' \itemize{
#'  \item{Test 1: Tmax(0) < Tmin(0) - 1}
#'  \item{Test 2: Tmean(0) > Tmax(0) + 1}
#'  \item{Test 3: Tmean(0) < Tmin(0) - 1}
#'  \item{Test 4: Tmax(0) < Tmin(1) - 1}
#'  \item{Test 5: Tmin(0) > Tmax(1) + 1}
#'  \item{Test 6: Tmax(1) < Tmean(0) - 1}
#'  \item{Test 7: Tmin(1) > Tmean(1) + 1}
#' }
#' 
#' The + / - 1 tolerance is added for tolerance if different thermometers are used
#' for the measurement of the variables.
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 5 "Internal and temporal consistency checks" and "Appendix A".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @return data.frame with two columns and same amount of rows as in \code{weather}. 
#' One column is for the flag of minimum temperature, the other for maximum temperature.
#' Values of \code{TRUE} indicate successful test, meaning that the tested 
#' variable exceeded the limits of the test.
#' @examples test_iterative_temperature_consistency(weather = target_weather)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
test_iterative_temperature_consistency <- function(weather){
  
  #this objects determines how long the while loop goes, start value is arbetrary and just chosen, so that the while loop runs at least one time
  max_violations <- 2
  
  #object to save the flags
  tmin_flag <- tmax_flag <- tmean_flag <- rep(FALSE, nrow(weather))
  
  while(max_violations > 1){
    #check which observations are available
    tmin0 <- is.na(weather$Tmin) == FALSE
    tmin1 <- is.na(dplyr::lead(weather$Tmin)) == FALSE
    tmax0 <- is.na(weather$Tmax) == FALSE
    tmax1 <- is.na(dplyr::lead(weather$Tmax)) == FALSE
    tmean0 <- is.na(weather$Tmean) == FALSE
    
    #objects to count the amount of positive tests 
    tmin_violations <- tmax_violations <- tmean_violations <- rep(0, nrow(weather))
    
    #1
    t1 <- ifelse(tmax0 & tmin0, yes = weather$Tmax < (weather$Tmin), no = FALSE)
    
    #2
    t2 <- ifelse(tmean0 & tmax0, yes = ifelse(weather$Tmean > (weather$Tmax), yes = TRUE, no = FALSE), no = FALSE)
    
    #3
    t3 <- ifelse(tmean0 & tmin0, yes = weather$Tmean < (weather$Tmin), no = FALSE)
    
    #4
    t4 <- ifelse(tmax0 & tmin1, yes = (weather$Tmax < (dplyr::lead(weather$Tmin) - 1)), no = FALSE)
    
    #5
    t5 <- ifelse(tmin0 & tmax1, yes = weather$Tmin > (dplyr::lead(weather$Tmax) + 1), no = FALSE)
    
    #6
    t6 <- ifelse(tmax1 & tmean0, yes = dplyr::lead(weather$Tmax) < (weather$Tmin -1), no = FALSE)
    
    #7
    t7 <- ifelse(tmin1 & tmean0, yes = dplyr::lead(weather$Tmin) > (weather$Tmean +1), no = FALSE)
    
    #count the amount of positive tests per reading, account for lead values
    tmax_violations <- t1 + t2 + t4 + dplyr::lag(t5) + dplyr::lag(t6)
    tmin_violations <- t1 + t3 + t5 + dplyr::lag(t4) + dplyr::lag(t7)
    tmean_violations <- t2 + t3 + t6 + t7
    
    #identify max amount of violations
    max_violations <- max(tmax_violations, tmin_violations, tmean_violations, na.rm = T)
    
    #mark in flag objects which entries will be removed
    tmin_flag[tmin_violations == max_violations] <- TRUE
    tmax_flag[tmax_violations == max_violations] <- TRUE
    tmean_flag[tmean_violations == max_violations] <- TRUE
    
    #remove objects from weather
    weather$Tmin[tmin_flag] <- NA
    weather$Tmax[tmax_flag] <- NA
    weather$Tmean[tmean_flag] <- NA
  }
  
  return(data.frame(tmin_flag, tmean_flag, tmax_flag))
  
}