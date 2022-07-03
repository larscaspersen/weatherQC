#' Test for exceedence of world records
#' 
#' Wrapper function to test for weather observations outside records of the country / region.
#' 
#' This function either retrieves the highest and lowest temperature (degree C) and 
#' daily precipitation (mm) for a specified country / subregion or uses user-defined 
#' limits. Then it tests which days of the specified variable in the weather data.frame
#' exceeds the limits and flags these.
#' 
#' This test is included in the weather quality control schemes \code{durre_qc()};
#' and \code{costa_qc()}. For more details on the weather quality control functions either 
#' refer to the help pages of the functions or see Costa et al. (2021) \insertCite{costa_gap_2021}{weather_QC}
#' or Durre et al. (2010) \insertCite{durre_comprehensive_2010}{weather_QC}.
#'
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable a character indicating the column name of the tested variable in weather
#' @param region a character indicating the region for which the records should be downloaded. 
#' Valid options are `world` and `USA`. Can be also set as \code{NULL} in case user-defined 
#' limits are supplied
#' @param subregion a character, only needed in case \code{region} is defined. In case of 
#' \code{region = "world"} it should indicate the country the weather data is from. 
#' In case of \code{region = "USA"} it should be the state's name the weather is obtained from.
#' If user-defined limits are used, should be set to \code{NULL}
#' @param records by default set to \code{NULL}. In case of user-defined limits, it should
#' be a numeric vector of length two, containing lower and upper limits of tested variable.
#' @return Logical vector of same length as rows in \code{weather}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test.
#' @seealso \code{\link{get_weather_records}}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' test_fixed_limit(weather = KA_weather, region = 'world', subregion = 'Germany', 
#' variable = "Tmin")
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}

test_fixed_limit <- function(weather, variable, region, subregion,  records = NULL){
  
  #checks for input stuff
  if(is.list(weather) == F){
    stop('weather needs to be a dataframe / tibble / data.table.')
  }
  
  if(is.null(region) & is.null(records)){
    stop('either region or records for variable need to be supplied. While one')
  } else if(is.null(region) == F & is.null(records) == F){
    warning('region and records were supplied. in this case the values of records will be used for the test')
  } 
  
  if(is.null(region) == F & is.null(subregion) == F){
    
    if(length(region) != 1){
      stop('region needs to be of length 1')
    }
    
    #check if valide region is chosen
    valid_regions <- c('world', 'USA')
    if(!region %in% valid_regions){
      stop(paste0('unvalid name for region You can only use: ', valid_regions, ' if the region you need is not available, then you need to research the records and supply them via "records"'))
    } 
    
    #download record data
    records <- get_weather_records(region = region)
    
    #check if the needed subregion / state is present
    if(! subregion %in% records$Country){
      stop(paste0('provided subregion name: ', subregion, ' could not be found in the records list. subregion list contains: ', paste0(unlist(records$subregion), collapse = ', ')))
    } else{
      #extract subregion-specific record
      records <-  filter(records, Country == subregion)
      
      if(variable %in% c('Tmin', 'Tmax')){
        records <- c(records$Tmin, records$Tmax)
      } else if(variable == 'Precip'){
        records <- c(0, records$Precip)
      } else{
        stop('Wrong variable supplied. Must be either Tmin, Tmax or Precip')
      }
      
      #check if records contain NA, in that case stop
      if(any(is.na(records))){
        stop('Records contain at least one NA. You need to specify records in that case via the "records" variable in the function call. records needs to contain upper and lower bound of variable')
      }
    }
    
  }
  
  flag <- ifelse(is.na(weather[,variable]),yes = FALSE, no = (weather[,variable] < records[1]) | (weather[,variable] > records[2]))
  
  return(as.logical(flag))
  
}