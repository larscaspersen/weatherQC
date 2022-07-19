#' Download temperature and precipitation records
#' 
#' This function scraps the highest and lowest daily temperature (degree C) and in some cases the highest daily precipitation (mm) measurments. 
#' 
#' In case of \code{region = "world"} the data will be retrieved from \href{https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_extreme_temperatures}{Wikipedia}. 
#' The returned tibble contains country-specific temperature records, which might be incomplete. Furthermore, on the country-level
#' it contains no information on precipitation records.
#' In case of \code{region = "USA"} the data will be retrieved from the \href{https://www.ncdc.noaa.gov/extremes/scec}{State Climate Extremes Committee (SCEC)}. 
#' The records are state-wise listed and contain temperature extremes records and precipitation records.
#'
#' @param region Character indicating for which level of region the data should be downloaded. Currently only "world" and "USA" are valid options.
#' @return Tibble containing the country-/ subregion-level daily minimum temperature, maximum temperature and precipitation records.
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @examples 
#' get_weather_records <- function(region = "world")
#' @export
get_weather_records <- function(region = "world"){
  

  #hide from cmd run
  . <- NULL
  
  if(region == 'world'){
    
    #url from which data is scrapped
    url <- 'https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_extreme_temperatures'
    
    #scrap data
    temperature_html <- rvest::read_html(url)
    
    #processes scrapped data
    temp_table <- temperature_html %>% 
      rvest::html_nodes(css = "table") %>% 
      dplyr::nth(1) %>% 
      rvest::html_table(fill = TRUE, convert = F)
    
    #extract lowest temeprature record
    cold <- readr::parse_number(temp_table$Coldest)
    
    #cases where we have negative value need special treatment
    #because of of special minus character, which confuses R
    neg_val <- stringr::str_detect(temp_table$Coldest, pattern = rlang::chr_unserialise_unicode('<U+2212>'))
    cold[neg_val] <- cold[neg_val] * -1
    
    #take hottest temperature
    warm <- readr::parse_number(temp_table$Hottest)
    
    #cases where we have negative value: strip the weird minus sign and add the 'real' minus
    neg_val <- stringr::str_detect(temp_table$Hottest, pattern = rlang::chr_unserialise_unicode('<U+2212>'))
    warm[neg_val] <- warm[neg_val] * -1
    
    
    #adjust and clean name of country
    temp_table$`Country/Region` <-  gsub(temp_table$`Country/Region`, pattern = '\\*', replacement = '') %>%
      stringr::str_trim()
    
    #final product, returned by function
    records <- tibble::tibble(Country = temp_table$`Country/Region`,
                      Tmin = cold,
                      Tmax = warm,
                      Precip = NA)
    
  }
  
  if(region == 'USA'){
    
    #download record temperatures (and rainfall)
    records <- data.table::fread('https://www.ncdc.noaa.gov/extremes/scec/records.csv',
                                 showProgress = FALSE)
    
    #take min and max temperature, change from fahrenheit to degree celsius
    records <- records %>%
      dplyr::select(.data$State, .data$Element, .data$Value) %>%
      dplyr::filter(.data$Element %in% c('All-Time Maximum Temperature', 
                                         'All-Time Minimum Temperature', 
                                         'All-Time Greatest 24-Hour Precipitation')) %>%
      dplyr::mutate('Element' = as.factor(.data$Element)) %>%
      dplyr::mutate('Element' = dplyr::recode_factor(.data$Element, 
                                                     'All-Time Minimum Temperature' = 'Tmin', 
                                     'All-Time Maximum Temperature' = 'Tmax',
                                     'All-Time Greatest 24-Hour Precipitation' = 'Precip')) %>%
      .[!duplicated(.),] %>%
      reshape2::dcast(State ~ Element, value.variable = 'Value', value.var = 'Value') %>%
      dplyr::mutate('Tmin' = round((as.numeric(.data$Tmin) - 32) * (5/9), digits = 1),
             'Tmax' = round((as.numeric(.data$Tmax) - 32) * (5/9), digits = 1),
             'Precip' = .data$Precip * 25.4)
    
    
    records <- tibble::as_tibble(records)
  }
  
  names(records) <- c('Country', 'Tmin', 'Tmax', 'Precip')
  return(records)
}
