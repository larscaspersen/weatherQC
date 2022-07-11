#' Identify instances of duplicated weather records
#' 
#' The function performs several tests to identify duplicated months / years
#' within the weather data.
#' 
#' The function tests there are instances of duplicated years (Precipitation),
#' duplicated months within the same year (Temperature and precipitation),
#' duplicated months among the same month of different years (temperature and precipitation)
#' and cases of Tmin = Tmax. In case of detected duplicates, all observation
#' of the tested variable belonging to the duplicated period are flagged. In case of
#' precipitation a period needs to contain at least three non-zero precipitation
#' events in order to be included in the duplication tests. Otherwise these periods
#' are skipped.
#' 
#' This function is part of the weather quality control scheme after \insertCite{durre_comprehensive_2010;textual}{weatherQC}. For more details 
#' please refer to section 3 "Basic integrity tests".
#' @param weather data.frame containing a daily time series data set. 
#' It should have columns c("Year", "Month", "Day")
#' @param variable column name in \code{weather} for which the test is performed. Should
#' be either Tmin or Tmax. data.frames in \code{aux_list} need to have the same
#' name
#' @param precip_min_nonzero minimum amount of non-zero precipitation events for
#' a period to be considered in the test, otherwise the period is skipped
#' @param same_temp_threshold threshold for a month to contain same Tmin as Tmax
#' observation to be flagged.
#' @return logical vector of same length as \code{nrow(weather)}. Values of \code{TRUE} indicate successful test,
#' meaning that the tested variable exceeded the limits of the test and is flagged
#' as suspicious
#' @examples get_duplicated_values(weather = target_weather, variable = "Tmin")
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @export
get_duplicated_values <- function(weather, variable, precip_min_nonzero = 3,
                                  same_temp_threshold = 10){
  
  #instanice the flags
  flag_dup_year <- flag_dup_mon <- flag_dup_mon2 <- flag_ident_temp <- rep(F, nrow(weather))
  
  ####
  #duplicated years
  ####
  
  #only applicaple for precipitation
  
  if(variable == 'Precip'){
    #only take years which have at least three non-zero precipitation events
    invest_years <- weather %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(n_nonzero = sum(Precip > 0, na.rm =T)) %>%
      dplyr::filter(n_nonzero >= 3) %>%
      dplyr::select(Year) %>%
      unlist()
    
    #check if the years to be investigated are duplicated (at least some of them)
    t1 <- split(weather[weather$Year %in% invest_years, variable], weather[weather$Year %in% invest_years, 'Year']) %>%
      duplicated()
    
    if(any(t1)){
      #duplcated only marks the second object as duplicated, so do the same test but from other direction
      #and take the union of both
      t1 <- t1 | split(weather[weather$Year %in% invest_years,variable], weather[weather$Year %in% invest_years, 'Year']) %>%
        duplicated(fromLast=TRUE)
      
      #filter the positive tested years
      sus_year <- invest_years[t1]
      
      #flag them as suspicious
      flag_dup_year <- weather$Year %in% sus_year
    }
    
    
  }
  
  
  ####
  #duplicated month within same year
  ####
  
  #add period to weather df, used to subset using year and month at the same time
  weather$period <- paste(weather$Year, weather$Month, sep = ' ')
  
  #copy of weather, needed because in case of precipitation it will be subsetted to 
  #months which at least contain three non-zero prcipitation events
  org_weather <- weather
  
  #for precipitation: find months with at least three non-zero precipitation events
  if(variable == 'Precip'){
    invest_periods <- weather %>%
      dplyr::group_by(Year, Month) %>%
      dplyr::summarise(n_nonzero = sum(Precip > 0, na.rm =T)) %>%
      dplyr::filter(n_nonzero >= 3) %>%
      dplyr::mutate(period = paste(Year, Month, sep = ' ')) %>%
      dplyr::pull(period)
    
    #subset weather by invest period
    weather <- weather[weather$period %in% invest_periods,]
  }
  
  
  #check if there is in one year a duplicated month
  t2 <- split(weather, weather$Year) %>%
    purrr::map_lgl(function(x) any(duplicated(split(x[,variable], x$Month))))
  
  if(any(t2)){
    #in case there is duplictaed month, find out which year
    sus_year <- split(weather, weather$Year) %>%
      purrr::map_lgl(function(x) any(duplicated(split(x[,variable], x$Month)))) %>%
      which() %>%
      unique(weather$Year)[.]
    
    #for this year(s) check which months are duplicated
    res1 <- weather %>%
      dplyr::filter(Year %in% sus_year) %>%
      split(.$Year) %>%
      purrr::map(function(x) duplicated(split(x[,variable], x$Month))) %>%
      purrr::map(which)
    
    res2 <-weather %>%
      dplyr::filter(Year %in% sus_year) %>%
      split(.$Year) %>%
      purrr::map(function(x) duplicated(split(x[,variable], x$Month), fromLast=TRUE)) %>%
      purrr::map(which)
    
    #subset for months which yielded duplications
    months <- map2(res1, res2, c) %>%
      purrr::map(unique) %>%
      unlist()
    
    years <- substr(names(months),1,4) %>%
      as.numeric()
    
    #create a combined variable to subset
    periods <- paste(years, months, sep = ' ')
    
    #create flag and change it to true in appropriate cases
    flag_dup_mon <-  org_weather$period %in% periods
    
  }
  
  #####
  #duplicated month among same months of different years
  #####
  
  t3 <- split(weather, weather$Month) %>%
    purrr::map_lgl(function(x) any(duplicated(split(x[,variable], x$Year))))
  
  if(any(t3)){
    #identify month of duplicates
    sus_month <- split(weather, weather$Month) %>%
      purrr::map_lgl(function(x) any(duplicated(split(x[,variable], x$Year)))) %>%
      which() %>%
      unique(weather$Month)[.]
    
    #identify which year of the months is problematic
    
    #for this year(s) checj which months are duplciated
    res1 <- weather %>%
      dplyr::filter(Month %in% sus_month) %>%
      split(.$Month) %>%
      purrr::map(function(x) duplicated(split(x[,variable], x$Year))) %>%
      purrr::map(which)
    
    res2 <-weather %>%
      dplyr::filter(Month %in% sus_month) %>%
      split(.$Month) %>%
      purrr::map(function(x) duplicated(split(x[,variable], x$Year), fromLast=TRUE)) %>%
      purrr::map(which)
    
    #subset for months which yielded duplications
    years <- map2(res1, res2, c) %>%
      purrr::map(unique) %>%
      purrr::map(function(x) unique(weather$Year)[x]) %>%
      unlist()
    months <- substr(names(years),1,nchar(names(years))-1) %>%
      as.numeric()
    
    #create a combined variable to subset
    periods <- paste(years, months, sep = ' ')
    weather$period <- paste(weather$Year, weather$Month)
    
    #create flag and change it to true in appropriate cases
    flag_dup_mon2 <-  org_weather$period %in% periods
    
  }
  
  
  #####
  #tmin == tmax of at least 10 or more --> flag whole month
  #####
  
  if(variable %in% c('Tmin', 'Tmax')){
    sus_period <- weather %>%
      dplyr::group_by(Year, Month) %>%
      dplyr::summarise(same_temp = sum(Tmin == Tmax, na.rm = T)) %>%
      dplyr::mutate(period = paste(Year, Month, sep = ' ')) %>%
      dplyr::filter(same_temp >= same_temp_threshold) %>%
      dplyr::pull(period)
    
    #in case of at least one incident, change the flag
    if(length(sus_period) > 0){
      flag_ident_temp <- org_weather$period %in% sus_period
    }
    
  }
  
  #combine the different flags
  flag <- flag_dup_year | flag_dup_mon | flag_dup_mon2 | flag_ident_temp
  
  #change nas to false
  flag[is.na(flag) == TRUE] <- FALSE
  
  #remove cases in which NA-only months where flagged
  flag <- ifelse(is.na(org_weather[[variable]]) == T & flag, yes = FALSE, no = flag)
  
  
  return(flag)
}
