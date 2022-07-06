#' Helper function for the weather quality control after \insertCite{durre_comprehensive_2010;textual}{weatherQC}
#' 
#' The function adds quality flags to the weather data and adds a note which
#' test led to the flagging of the observation.
#' 
#' This function is needed because of the iterative testing framework of the 
#' weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. At the beginning
#' robust tests are carried out, which are not affected by strong outliers,
#' like a -9999 for example. But following test which take the long term mean or 
#' long-term percentiles into account can be rendered useless if such strong
#' outliers are present in the weather data. So they are removed by previous tests
#' (but remain in the column "variable_org"), where variable is the tested 
#' column.
#' 
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable column name in \code{weather} for which the test is performed. Should
#' be either Tmin or Tmax. data.frames in \code{aux_list} need to have the same
#' name
#' @param test_result logical vector of same length as \code{nrow(weather)}
#' @param test_name character, adds note to in the column "flag_variable", where
#' variable is the same as in \code{variable}
#' percentiles for the corresponding day of the year
#' @return data.frame with same rows as \code{weather}. Observations which were
#' flagged are removed from weather and replaced by NA's. A note which test lead
#' to the removal is added in a seperate column.
#' \code{percentile_df}, then it returns TRUE, otherwise FALSE 
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @examples 
#' #add needed columns to weather
#' weather <- tibble::tibble(weather, 
#'                           'Tmin_org' = weather$Tmin, 
#'                           'Tmax_org' = weather$Tmax, 
#'                           'Precip_org' = weather$Precip,
#'                           'flag_Tmin' = NA, 
#'                           'flag_Tmax' = NA, 
#'                           'flag_Precip' = NA)
#'                           
#' #perform test
#' test_result <- test_spike_dip(weather = weather, variable = "Tmin")
#' 
#' #add test result to weather
#' weather <- clear_flagged_data(weather = weather, variable = "Tmin",
#'                               test_result = test_result,
#'                               test_name = "spike_dip")
clear_flagged_data <- function(weather, variable, test_result, test_name){
  #set values to NA for positive test results
  weather[test_result, variable] <- NA
  
  #indicate which test lead to removal
  weather[test_result, paste0('flag_', variable)] <- test_name
  
  return(weather)
}