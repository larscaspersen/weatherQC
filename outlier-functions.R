#scrap temperature records from wikipedia
#follow example of https://towardsdatascience.com/scraping-data-from-wikipedia-tables-3efa04c6b53f
library(tidyverse)

#function to get temperature records
get_temp_records <- function(region = 'world'){
  
  if(region == 'world'){
    
    url <- 'https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_extreme_temperatures'
    
    temperature_html <- rvest::read_html(url)
    
    temp_table <- temperature_html %>% 
      rvest::html_nodes(css = "table") %>% 
      nth(1) %>% 
      rvest::html_table(fill = TRUE, convert = F)
    
    cold <- readr::parse_number(temp_table$Coldest)
    
    #cases where we have negative value
    neg_val <- str_detect(temp_table$Coldest, pattern = rlang::chr_unserialise_unicode('<U+2212>'))
    cold[neg_val] <- cold[neg_val] * -1
    
    warm <- readr::parse_number(temp_table$Hottest)
    
    #cases where we have negative value
    neg_val <- str_detect(temp_table$Hottest, pattern = rlang::chr_unserialise_unicode('<U+2212>'))
    
    warm[neg_val] <- warm[neg_val] * -1

    
    #adjust and clean name of country
    temp_table$`Country/Region` <-  gsub(temp_table$`Country/Region`, pattern = '\\*', replacement = '') %>%
      str_trim()
    
    records <- tibble(Country = temp_table$`Country/Region`,
                          Tmin = cold,
                          Tmax = warm,
                          Precip = NA)
    
  }
  
  if(region == 'USA'){
    
    #download record temperatures (and rainfall)
    records <- data.table::fread('https://www.ncdc.noaa.gov/extremes/scec/records.csv')
    
    #take min and max temperature, change from fahrenheit to degree celsius
    records <- records %>%
      select(State, Element, Value) %>%
      filter(Element %in% c('All-Time Maximum Temperature', 'All-Time Minimum Temperature', 'All-Time Greatest 24-Hour Precipitation')) %>%
      mutate(Element = as.factor(Element)) %>%
      mutate(Element = recode_factor(Element, 'All-Time Minimum Temperature' = 'Tmin', 
                                     'All-Time Maximum Temperature' = 'Tmax',
                                     'All-Time Greatest 24-Hour Precipitation' = 'Precip')) %>%
      .[!duplicated(.),] %>%
      reshape2::dcast(State ~ Element, value.var = 'Value') %>%
      mutate(Tmin = round((as.numeric(Tmin) - 32) * (5/9), digits = 1),
             Tmax = round((as.numeric(Tmax) - 32) * (5/9), digits = 1),
             Precip = Precip * 25.4)
    

    records <- as_tibble(records)
  }
  
  names(records) <- c('Country', 'Tmin', 'Tmax', 'Precip')
  return(records)
}



###fixed limits test

fixed_limit_test <- function(weather, level, var, country,  records = NULL){
  
  #checks for input stuff
  if(is.list(weather) == F){
    stop('weather needs to be a dataframe / tibble / data.table.')
  }
  
  if(is.null(level) & is.null(records)){
    stop('either region or records for variable need to be supplied. While one')
  } else if(is.null(level) == F & is.null(records) == F){
    warning('region and records were supplied. in this case the values of records will be used for the test')
  } 
  
  if(is.null(level) == F & is.null(country) == F){
    
    if(length(level) != 1){
      stop('region needs to be of length 1')
    }
    
    #check if valide region is chosen
    valid_regions <- c('world', 'USA')
    if(!level %in% valid_regions){
      stop(paste0('unvalid name for level You can only use: ', valid_regions, ' if the level you need is not available, then you need to research the records and supply them via "records"'))
    } 
    
    #download record data
    records <- get_temp_records(region = level)
    
    #check if the needed country / state is present
    if(! country %in% records$Country){
      stop(paste0('provided country name: ', country, ' could not be found in the records list. Country list contains: ', paste0(unlist(records$Country), collapse = ', ')))
    } else{
      #extract country-specific record
      records <-  filter(records, Country == country)
      
      if(var %in% c('Tmin', 'Tmax')){
        records <- c(records$Tmin, records$Tmax)
      } else if(var == 'Precip'){
        records <- c(0, records$Precip)
      } else{
        stop('Wrong var supplied. Must be either Tmin, Tmax or Precip')
      }
      
      #check if records contain NA, in that case stop
      if(any(is.na(records))){
        stop('Records contain at least one NA. You need to specify records in that case via the "records" variable in the function call. records needs to contain upper and lower bound of variable')
      }
    }
    
  }
  
  flag <- ifelse(is.na(weather[,var]),yes = FALSE, no = (weather[,var] < records[1]) | (weather[,var] > records[2]))
  
  return(as.logical(flag))
  
}


###variable limit test

variable_limit_test <- function(weather, var, weather_coords, probs = c(0.01, 0.99),
                                resolution = 0.5){
  
  #### download long term monthly means for the location
  
  #download weather data (if already downloaded the function will detect it and not download it a second time)
  r <- raster::getData("worldclim",var=tolower(var),res= resolution, 
               lon = weather_coords[1], lat = weather_coords[2])
  
  spatial_df <- sp::SpatialPoints(cbind(weather_coords[1], weather_coords[2]))
  
  #extract climate normals of worldclim data
  values <- raster::extract(r,spatial_df)
  values <- values / 10

  #remove downloaded geospatial data
  rm(r)
  
  
  #### create quantiles of deviation from monthly means 
  
  #split to monthly groups
  monthly_weather <- split(weather, f = weather$Month)
  
  #get monthly percentiles
  monthly_test <- purrr::map(monthly_weather, ~quantile(.x[,var], probs = c(probs[1], probs[2]), na.rm = T)) %>%
    purrr::map2(monthly_weather, function(x,y)ifelse(is.na(y[,var]),yes = F, no = ((y[,var]<x[1])|y[,var]>x[2]))) %>%
    purrr::map2(monthly_weather, cbind) %>%
    do.call(rbind, .) %>%
    arrange(Date) %>%
    .[,1]
  
  #also test for yearly quantiles
  yearly_test <- quantile(weather[,var], probs = c(probs[1], probs[2]), na.rm = T) %>%
    {ifelse(is.na(weather[,var]), yes = F, no = ((weather[, var] < .[1])|(weather[,var]>.[2])))}
  
  return(yearly_test | monthly_test)

}

#function to add date column 
add_date <- function(x){
  x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = '%Y-%m-%d')
  return(x)
}


###temporal continuity
#calculate absolute difference to next day
#add doy to weather

temporal_continuity_test <-  function(weather, var, prob = 0.995){
  #calculate difference to next day, append one NA to keep same length
  diffs <- c(abs(diff(as.matrix(weather[,var]))), NA)
  
  #get the quantile of the jumps, everything above the quantile is labelled as suspicious
  jump_quan <- quantile(diffs, probs = prob, na.rm = T)
  
  return(ifelse(is.na(diffs), yes = F, no = (diffs > jump_quan)))
  
}


#### consitstency between variables (I ignore the mean temperature for now)
temperature_consistency_test <- function(weather, probs = 0.99){
  #minimum temperature should not be higher then maximum temperature (also one day earlier or later)
  cons1 <- (weather$Tmin >= weather$Tmax) | (weather$Tmin >= dplyr::lag(weather$Tmax)) | (weather$Tmin >= dplyr::lead(weather$Tmax))

  #min temperature should be lower than tmean
  cons2 <- (weather$Tmin >= weather$Tmean) | (weather$Tmin >= dplyr::lag(weather$Tmean)) | (weather$Tmin >= dplyr::lead(weather$Tmean))
  
  #tmean should not be too different from (tmin + tmax)/2
  diff_tmean <- abs(((weather$Tmin + weather$Tmax)/2) - weather$Tmean)
  perc_diff_tmean <- quantile(diff_tmean, prob = probs, na.rm = T)
  cons3 <- diff_tmean > perc_diff_tmean
  
  
  return(ifelse(is.na(weather[,var]), yes = F, no = (cons1 | cons2 | cons3)))
  
}

#get ecdf of precipitation value of a certain window-width
get_ecdf <- function(weather, doy, min_non_zero_days = 20){
  #get doys of target days
  lim_doy <- doy + c(-14,14)
  
  #adjust for lower lims in old year
  lim_doy <- ifelse(lim_doy <= 0, yes = lim_doy + 365,no = lim_doy)
  
  #adjsut for upper lims in new year
  lim_doy <- ifelse(lim_doy >= 365, yes = lim_doy - 365, no = lim_doy)
  
  
  #if the range of days falls inbetween two years, than different subsetting needed
  if(lim_doy[1] > lim_doy[2]){
    
    target_days <- weather$doy >= lim_doy[1] | weather$doy <= lim_doy[2]
    
  } else {
    
    target_days <- weather$doy >= lim_doy[1] & weather$doy <= lim_doy[2]
  }
  
  #check if at least 20 non_zero values are present
  if(sum(weather$Precip[target_days] > 0, na.rm = T) < min_non_zero_days){
    return(NA)
  }
  
  #take non zero precipitation data from the 29 window over all years, 
  #make it a empirical cumulative distribution function, then determine 
  #the percentile of the valuz 
  return(weather$Precip[target_days] %>%
           na.omit() %>%
           .[.>0] %>%
           ecdf())
  
}

#get percetniles of precipitation
get_clim_percentiles_prec <- function(weather, doy, probs = c(0.3,0.5,0.7,0.9),
                                      min_non_zero_days = 20){
  #get doys of target days
  lim_doy <- doy + c(-14,14)
  
  #adjust for lower lims in old year
  lim_doy <- ifelse(lim_doy <= 0, yes = lim_doy + 365,no = lim_doy)
  
  #adjsut for upper lims in new year
  lim_doy <- ifelse(lim_doy >= 365, yes = lim_doy - 365, no = lim_doy)
  
  
  #if the range of days falls inbetween two years, than different subsetting needed
  if(lim_doy[1] > lim_doy[2]){
    
    target_days <- weather$doy >= lim_doy[1] | weather$doy <= lim_doy[2]
    
  } else {
    
    target_days <- weather$doy >= lim_doy[1] & weather$doy <= lim_doy[2]
  }
  
  #check if at least 20 non_zero values are present
  if(sum(weather$Precip[target_days] > 0, na.rm = T) <= min_non_zero_days){
    return(NA)
  }
  
  #take non zero precipitation data from the 29 window over all years, 
  #make it a empirical cumulative distribution function, then determine 
  #the percentile of the valuz 
  return(weather$Precip[target_days] %>%
           na.omit() %>%
           .[.>0] %>%
           quantile(probs = probs))
  
}


#get percentile of precipitation data
get_prec_rank <- function(weather,min_non_zero_days = 20){
  
  #add date and doy
  weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep ='-'),
                          format = '%Y-%m-%d')
  weather$doy <- lubridate::yday(weather$Date)
  
  
  
  ecdf_list <- map(unique(weather$doy), ~ get_ecdf(weather = weather, doy = .x, min_non_zero_days = min_non_zero_days))
  
  return(map2(split(weather,f = weather$doy), ecdf_list, function(x,y) floor(y(x$Precip) * 100)) %>%
           unsplit(weather$doy))
  
}

#get absolute minimum difference either for precipitation or precipitation percentile rank
get_abs_min_difference <- function(weather,i, var, aux_list){
  
  if(is.na(weather[i, var]) == TRUE){
    return(NA)
  }
  
  #extract values from aux station
  int <- map_dbl(aux_list, function(x) x[[i, var]])
  if(i < nrow(weather)){
    int <- c(int, map_dbl(aux_list, function(x) x[[i+1, var]]))
  }
  if(i != 1){
    int <- c(int, map_dbl(aux_list, function(x) x[[i-1, var]]))
  }
  
  #if the value is not the highest or the lowest, then there is no need to carry out 
  #the corrobation test
  if(var == 'Precip'){
    if(all(weather[i,var] > int, na.rm = T) | all(weather[i, var] < int, na.rm = T) == F){
      return(0)
    } else{
      return(min(abs(weather[i,var] - int), na.rm =T))
    }
  } else{
    return(min(abs(weather[i,var] - int), na.rm = T))
  }
}



precipitation_spatial_corrobation_test <- function(weather, weather_coords, aux_info, aux_list,
                                                   max_dist = 75,
                                                   max_station = 7, min_station = 3){
  
  #this should be done ONCE, it happens also in the temperature spatial consistency test
  
  #calculate distance to aux_stations
  aux_info$dist <-  round(sp::spDistsN1(pts = as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                        pt = weather_coords, longlat = TRUE), 2)
  
  #select stations within the max distance, which are not the target station
  aux_info <- aux_info %>%
    filter(dist > 0 & dist <= max_dist) %>%
    arrange(dist)
  
  #if too few neighbouring values, then the test can't be carried out
  if(nrow(aux_info) < min_station){
    return(rep(NA, nrow(weather)))
  }  else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  } 
  aux_list <- aux_list[aux_info$id]
  
  ####
  #calculate prec rank
  ####
  
  #calculate for each precipitation value the percentile rank
  weather$prec_rank <- get_prec_rank(weather)
  
  #also add prec rank to aux data
  aux_list <-  map(aux_list, get_prec_rank) %>%
    map2(., aux_list, function(x,y) tibble(y, prec_rank = x))
  
  ####
  #get absolute minimum difference
  ####
  
  #now determine the prec rank difference  
  prec_min_difference <- imap_dbl(weather$Precip, ~ get_abs_min_difference(weather = weather, i = .y, var = 'Precip', aux_list = aux_list))
  #same for precipitation percentile rank
  prec_rank_difference <-  imap_dbl(weather$Precip, ~ get_abs_min_difference(weather = weather, i = .y, var = 'prec_rank', aux_list = aux_list))
  
  
  ###
  #get test threshold & and test
  ###
  test_threshold <- (-45.72 * log(prec_rank_difference) + 269.24)
  
  return(prec_min_difference > test_threshold)
  
}


select_target_days <- function(df, var, period_start, period_end){
  
  #if not present, add Date column
  df$Date <- as.Date(paste(df$Year, df$Month, df$Day, sep = '-'), format = '%Y-%m-%d')
  
  #in case every day of the target period is also preent in the weather dataframe, then simply reutrn everything
  if(all(period_start:period_end %in% df$Date)){
    return(df[df$Date >= period_start & df$Date <= period_end,var])
  } else{
    #make a placeholder for the selected days
    re_vec <- rep(NA, as.numeric(period_end-period_start +1))
    
    #change days which are present in the 
    re_vec[period_start:period_end %in% df$Date] <- df[df$Date >= period_start & df$Date <= period_end,var]
    
    return(re_vec)
    
  }
  
}

#calculate index of agreement between aux and target station
calc_index_agreement <- function(x,y){
  return(1 - ((sum(abs(y-x), na.rm = T))/(sum(abs(x - mean(y,na.rm = T)) + abs((y - mean(y, na.rm = T))),na.rm = T) )))
}

#helper function to get closest value of y in a three day window, given the observation in x

get_closest_y <-  function(x,y,i){
  #if x is NA or all of y, then return na
  if(is.na(x)) return(NA)
  if(all(is.na(y[(i-1):(i+1)]))) return(NA)
  
  #find closest y to x, if two are similar close then take the first
  return(y[(i-1):(i+1)][which(min(abs(y[(i-1):(i+1)] - x), na.rm =T) == (abs(y[(i-1):(i+1)] - x)))[1]])
}


#function to perform spatial consistency check for one period
spat_consist_one_period <- function(weather, aux_list, aux_info, period_start, var,
                                    max_res = 8, max_res_norm = 4, min_station = 3,
                                    max_station = 7, window_width = 15, 
                                    min_correlation = 0.8, min_coverage = 40){
  
  
  
  #check if period_start is date
  is.date <- function(x) inherits(x, 'Date')
  if(is.date(period_start) == F){
    stop('period_start needs to be supplied in date format. Try is_Date()')
  }
  
  
  
  #add window width to the period
  period_end <- lubridate::ceiling_date(period_start,unit = 'month') + window_width -1
  period_start <- period_start - window_width
  
  #extract data from target (x) and aux (y)
  x <- select_target_days(df = weather, var = var, period_start = period_start, period_end = period_end)
  y <- purrr::map(aux_list, ~ select_target_days(df = .x, var = var, period_start = period_start, period_end = period_end)) %>%
    do.call(cbind.data.frame, .)
  
  #only keep aux stations which fulfill coverage criteria
  aux_info <- aux_info[colSums(is.na(x) == F & is.na(y) == F) >= min_coverage, ]
  
  #drop stations from y
  y <- y[,aux_info$id]
  
  #calcualte index of agreement and sort decreasing
  aux_info$ind_agreement <- purrr::map_dbl(y, ~ calc_index_agreement(x = x, y = .x))
  aux_info <- aux_info[order(aux_info$ind_agreement,decreasing = T),]
  
  #bring y in same order
  y <- y[,aux_info$id]
  
  #iterate over all y columns, for each column iterate over x and find the closest y value given a 3 day window centered around i
  y_closest <-  purrr::map(y, function(vec) purrr::imap_dbl(x,~get_closest_y(x = .x, y=vec, i = .y))) %>%
    do.call(cbind.data.frame, .)
  
  #carry out linear regression per station with r
  coefs <- purrr::map(y_closest, ~ lm(x~.x)) %>%
    purrr::map(., function(x) c(x$coefficients[1], x$coefficients[2]))
    
  #calculate correlation coefficient, filter stations with too low correlation coefficient
  #keep only stations fulfillinf the criteria of minimum correlation of their prediction
  aux_info <- purrr::map(y_closest, ~ lm(x~.x)) %>%
      purrr::map_lgl(., ~sqrt(summary(.x)[['r.squared']]) > min_correlation) %>%
      aux_info[.,]
  

  #if there are less then 3 stations remaining, then return NAs as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  } else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  }
  
  coefs <-   coefs[aux_info$id]
  y_closest <- y_closest[,aux_info$id]
  
  #calculate weighted model estimates for each day
  #problem: nas are not returned here, I need to match it with x
  fitted_val <- purrr::map2(coefs, y_closest, .f = function(x,y){
    x[1] +  x[2] * y}) %>%
    bind_cols() %>%
    as.matrix()
  
  #get weighted mean of fitted values
  pred_x <- as.numeric((fitted_val %*% as.matrix(aux_info$ind_agreement) ) / sum(aux_info$ind_agreement))
  
  #object for residuals
  x_res <- rep(NA, length(x))
  x_res <- round(x - pred_x, digits = 2)
  
  #standardized residuals (by mean and std)
  x_res_norm <- (x_res - mean(x_res, na.rm = T)) / sd(x_res, na.rm = T)
  
  
  #take only the values for the month
  #--> strip the leading and trailing 15 values
  x_res <- x_res[(window_width+1):(length(x_res) - (window_width))]
  x_res_norm <- x_res_norm[(window_width+1):(length(x_res_norm) - (window_width))]
  
  flag_res <- ifelse(is.na(x_res), yes = F, no = abs(x_res) >= max_res)
  flag_res_norm <-  ifelse(is.na(x_res_norm), yes = F, no = abs(x_res_norm) >= max_res_norm)
  
  #if either the residuals or the standardized resiudals exceed the threshold, return for that given day a true
  return(flag_res | flag_res_norm)
  
}


#climatological outlier:
#calculate mean of each day of the year for a certain window width





#function for spatial consistency test, extract target period from weather data
#function prevents that if subscript is out of bounds that null is returned, instead ensures same length for all stations
spatial_consistency_test <- function(weather, weather_coords, aux_list, aux_info, 
                                     var, max_dist = 75, window_width = 75, 
                                     min_coverage = 40, min_correlation = 0.8,
                                     min_station = 3, max_station = 7, max_res = 8, 
                                     max_res_norm = 4){
  
  ##checks that the objects are of right type / format
  
  #weather needs to be a dataframe, var needs to 
  if(any(!purrr::map_lgl(list(weather, aux_list, aux_info), is_list))){
    stop('weather, aux_list and aux_info need to be lists. At least one of the objects is of wrong type')
  }
  
  #weather coords needs to be numeric and of length two
  if(is.numeric(weather_coords) == F | length(weather_coords) !=2){
    stop('weather coords needs to be numeric and of length two with longitude as first element and latitude as second element')
  }
  
  #required column names
  if(any(!c('Year', 'Month', 'Day', var) %in% colnames(weather))){
    missing <- c('Year', 'Month', 'Day', var)[!c('Year', 'Month', 'Day', var) %in% colnames(weather)]
    stop(paste0('Missing column names in weather. Columns: ', missing, ' missing'))
  }
  
  
  #names in aux_info and aux_list need to be the same and also of same length
  if(length(aux_list) != nrow(aux_info)){
    stop('aux_list and aux_info need to be of the same length')
  }
  
  #aux_info needs to contain columns id, latitude and longitude
  if(any(!c('id', 'Latitude', 'Longitude') %in% colnames(aux_info))){
    missing <- c('id', 'Latitude', 'Longitude')[!c('id', 'Latitude', 'Longitude') %in% colnames(aux_info)]
    stop(paste0('Missing column names in aux_info. Columns: ', missing, ' missing'))
  }
  
  #check that ids of aux info is the same as in names of aux_list
  if(any(!(names(aux_list) %in% aux_info$id))){
    stop('Names of aux_list need to be the same as id column in aux_info')
  }
  
  #check that the column names of aux_list are right
  if(any(purrr::map_lgl(aux_list, .f = function(x) any(!(c('Year', 'Month', 'Day', var) %in% colnames(x)))))){
    stop(paste0('weather records in aux_list need to contain at least the columns: Year, Month, Day and ', var, '. Please provide aux_list weather data in right format' ))
  }
  
  
  
  #calculate distance to aux_stations
  aux_info$dist <-  round(sp::spDistsN1(pts = as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                        pt = weather_coords, longlat = TRUE), 2)
  
  #select stations within the max distance, which are not the target station
  aux_info <- aux_info %>%
    filter(dist > 0 & dist <= max_dist)
  
  aux_list <- aux_list[aux_info$id]

  

  ###get vector of start and end dates for extraction period
  
  #get first and last month, create sequence of starting months, add extra days by window width
  first_month <- lubridate::floor_date(weather$Date[1], unit = 'month')
  last_month <- lubridate::floor_date(weather$Date[nrow(weather)], unit = 'month')
  starts <- seq(from = first_month, to = last_month, by = 'months')
  
  
  #calculate flags for each month
  spatial_flags <- purrr::map(starts, ~spat_consist_one_period(weather = weather, aux_list = aux_list,
                                               aux_info = aux_info, var = var, period_start = .x, 
                                               window_width = window_width, max_res = max_res, 
                                               max_res_norm = max_res_norm, min_station = min_station, 
                                               max_station = max_station, min_correlation = min_correlation, 
                                               min_coverage = min_coverage))
  
  #return the the flags in form of a list
  return(unlist(spatial_flags))
  
}  


#outlier test combined

test_for_outlier <- function(weather, weather_coords, var,
                             aux_list, aux_info, level, country, records = NULL,
                             probs_variable_limit = c(0.01, 0.99), resolution = 0.5,
                             probs_temporal_continuity = 0.995,
                             probs_temperature_consistency = 0.99,
                             max_dist = 75, window_width = 75, 
                             min_coverage = 40, min_correlation = 0.8,
                             min_station = 3, max_station = 7, max_res = 8, 
                             max_res_norm = 4){
  
  #if temperature, then Tmean is needed
  if(var %in% c('Tmin', 'Tmax')){
    if(!'Tmean' %in% colnames(weather)){
      stop('For variable consistency test daily average temperature called "Tmean" is required')
    }
  }
  
  if(!'Date' %in% colnames(weather)){
    weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                            format = '%Y-%m-%d')
  }

  #do testing of input stuff: 
  #   weather needs to have certain columns
  #   weather coords needs to be of length two and numeric
  #   var should be tmin, tmax (or preciptiation)
  #   aux_list needs to be list, names need to be same as in aux_info$id; colnames should contain same objects as weather does
  #   aux_info needs to contain coordinates and date?
  
  #call fixed limits test
  fixed_lim <- fixed_limit_test(weather = weather, level = level, 
                   country = country,var = var)
  
  #call variable limits test
  variable_lim <- variable_limit_test(weather = weather, var = var, weather_coords = weather_coords,
                                      probs = probs_variable_limit, resolution = resolution)
  
  #call temporal consistency test
  temporal_consistency <- temporal_continuity_test(weather = weather, var = var, 
                                                   prob = probs_temporal_continuity)
  
  #call variable consistency test (only applicable for temperature data)
  #make ifelse condition for temperature and precipitation
  if(var %in% c('Tmin', 'Tmax')){
    variable_consistency <- temperature_consistency_test(weather = weather,
                                                            probs = probs_temperature_consistency)
    
    spatial_consistency <- spatial_consistency_test(weather = weather, 
                                                    weather_coords = weather_coords, 
                                                    aux_list = aux_list, aux_info = aux_info, 
                                                    var = var, max_dist = max_dist, 
                                                    window_width = window_width, 
                                                    min_coverage = min_coverage, 
                                                    min_correlation = min_correlation,
                                                    min_station = min_station, 
                                                    max_station = max_station, max_res = max_res, 
                                                    max_res_norm = max_res_norm)
  } else if(var == 'Precip'){
    variable_consistency <- NA
    
    spatial_consistency <- precipitation_spatial_corrobation_test(weather = weather, 
                                                                  weather_coords = weather_coords,
                                                                  aux_info = aux_info,
                                                                  aux_list = aux_list,
                                                                  max_dist = max_dist,
                                                                  max_station = max_station,
                                                                  min_station = min_station)
  }
  
  
  
  test_res <- tibble(fixed_lim, variable_lim, temporal_consistency, variable_consistency, 
                    spatial_consistency)
  
  test_res$outlier <- rowSums(test_res, na.rm = T) >= 2
  
  return(test_res)
}





###
#outlier_detection_durre 
####

#naught check
# - tmin and tmax both either 0 or -17.8

#repetition
# -between years: all values of one year == values of another year
# -between different months of same year
# -same calendar month of different years
# -between TMAX and TMIN: Tmax == Tmin for at least 10 days or mor of a month

get_duplicated_values <- function(weather, var){
  
  #instanice the flags
  flag_dup_year <- rep(F, nrow(weather))
  flag_dup_mon <- rep(F, nrow(weather))
  flag_dup_mon2 <- rep(F, nrow(weather))
  
  ####
  #duplicated years
  ####
  t1 <- split(weather[,var], weather$Year) %>%
    duplicated()
  
  #if two years are duplicated, only one will be marked, use generic form and fromlast = T
  #to find ALL duplciates
  if(any(t1)){
    t1 <- t1 | split(weather[,var], weather$Year) %>%
      duplicated(fromLast=TRUE)
    
    sus_year <- unique(weather$Year)[t1]
    
    flag_dup_year <- weather$Year %in% sus_year
  }
  
  ####
  #duplicated month within same year
  ####
  
  #check if there is in one year a duplicated month
  t2 <- split(weather, weather$Year) %>%
    map_lgl(function(x) any(duplicated(split(x[,var], x$Month))))
  
  if(any(t2)){
    #in case there is duplictaed month, find out which year
    sus_year <- split(weather, weather$Year) %>%
      map_lgl(function(x) any(duplicated(split(x[,var], x$Month)))) %>%
      which() %>%
      unique(weather$Year)[.]
    
    #for this year(s) checj which months are duplciated
    res1 <- weather %>%
      filter(Year %in% sus_year) %>%
      split(.$Year) %>%
      map(function(x) duplicated(split(x[,var], x$Month))) %>%
      map(which)
    
    res2 <-weather %>%
      filter(Year %in% sus_year) %>%
      split(.$Year) %>%
      map(function(x) duplicated(split(x[,var], x$Month), fromLast=TRUE)) %>%
      map(which)
    
    #subset for months which yielded duplications
    months <- map2(res1, res2, c) %>%
      map(unique) %>%
      unlist()
    
    years <- substr(names(months),1,4) %>%
      as.numeric()

    #create a combined variable to subset
    periods <- paste(years, months, sep = ' ')
    weather$period <- paste(weather$Year, weather$Month)
    
    #create flag and change it to true in appropriate cases
    flag_dup_mon <-  weather$period %in% periods
    
  }
  
  #####
  #duplicated month among same months of different years
  #####
  
  t3 <- split(weather, weather$Month) %>%
    map_lgl(function(x) any(duplicated(split(x[,var], x$Year))))
  
  if(any(t3)){
    #identify month of duplicates
    sus_month <- split(weather, weather$Month) %>%
      map_lgl(function(x) any(duplicated(split(x[,var], x$Year)))) %>%
      which() %>%
      unique(weather$Month)[.]
    
    #identify which year of the months is problematic
    
    #for this year(s) checj which months are duplciated
    res1 <- weather %>%
      filter(Month %in% sus_month) %>%
      split(.$Month) %>%
      map(function(x) duplicated(split(x[,var], x$Year))) %>%
      map(which)
    
    res2 <-weather %>%
      filter(Month %in% sus_month) %>%
      split(.$Month) %>%
      map(function(x) duplicated(split(x[,var], x$Year), fromLast=TRUE)) %>%
      map(which)
    
    #subset for months which yielded duplications
    years <- map2(res1, res2, c) %>%
      map(unique) %>%
      map(function(x) unique(weather$Year)[x]) %>%
      unlist()
    months <- substr(names(years),1,nchar(names(years))-1) %>%
      as.numeric()
    
    #create a combined variable to subset
    periods <- paste(years, months, sep = ' ')
    weather$period <- paste(weather$Year, weather$Month)
    
    #create flag and change it to true in appropriate cases
    flag_dup_mon2 <-  weather$period %in% periods
    
  }
  
  #combine the different flags
  flag <- flag_dup_year | flag_dup_mon | flag_dup_mon2
  
  #change nas to false
  flag[is.na(flag) == TRUE] <- FALSE
  
  return(flag)
}





###repition checks


get_streaks <- function(weather, var, rep_threshold = 20){
  
  #only take variable of interest
  x <- weather[,c('Date', var)]
  names(x) <- c('Date', 'trials')
  #remove missing values
  x <- x[is.na(x$trials) == F,]
  
  #in case of precipitation: also remove zeros
  if(var == 'Precip'){
    x <- x[x$trials != 0,]
  }
  
  x <- x %>%  mutate(lagged=lag(trials)) %>% #note: that's dplyr::lag, not stats::lag
    mutate(start=(trials != lagged))
  x[1, "start"] <- TRUE
  x <- x %>% mutate(streak_id=cumsum(start))
  x <- x %>% group_by(streak_id) %>% mutate(streak=row_number()) %>%
    ungroup()
  
  #filter days which exceed the streak id
  sub <- x %>%
    filter(streak >= rep_threshold)
  
  #get all days belonging to the streaks, mark those days as suspicious
  sus_days <- x[x$streak_id %in% sub$streak_id,'Date']
  
  return(weather$Date %in% pull(sus_days))
}


#identical value frequent check
#only applies to precipitaiton

#helper function to check if a frequent value exceeds the threshold
helper_check_frequent_val <-  function(val, val_rep,doy, percentile_df){
  
  #but there can be the case that I have two times repitions of 5, 
  #in that case val and val_rep are of length 2
  #usual case
  
  if(val_rep >= 9){
    col_check <- "30%"
  } else if(val_rep >= 8){
    col_check <- "50%"
  } else if(val_rep >= 7){
    col_check <- "70%"
  } else if(val_rep >= 5){
    col_check <- "90%"
  } else{
    stop("Repitions not higher than the needed minimum of repitions (=5)")
  }
  
  #check if value is available in percentile df, if it is NA, return FALSE
  if(is.na(percentile_df[doy,col_check]) == T){
    return(FALSE)
  } else{
   
  #return test result, if val is greater or equal to percentile
   return(val >= as.numeric(percentile_df[doy,col_check])) 
  }
  

  
}

#only aplicable for precipitation
frequent_value_check <- function(weather, percentile_df, min_identical = 5, 
                                 min_non_zero_days = 20){
  
  #drop na values, and 0
  x <- weather %>%
    .[is.na(.$Precip) == FALSE,] %>%
    filter(Precip > 0)
  
  #flag 
  frequent_value_flag <- rep(F, nrow(x))
  
  for(i in 1:(nrow(x)-9)){
    
    #check for the day if there is a minimum of repitions
    if(any(table(x[i:(i+9),'Precip'])>=min_identical)){
      
      if(sum(table(x[i:(i+9),'Precip'])>=min_identical) == 1){
        #extract the value of repitions and check if the repeated value exceeds the limits of the test
        val_rep <- table(x[i:(i+9),'Precip'])[table(x[i:(i+9),'Precip'])>=min_identical]
        
        #look up if repeated value exceeds the threshold in percentile df
        if(helper_check_frequent_val(val = as.numeric(names(val_rep)), val_rep = val_rep, 
                              doy = weather$doy[i], percentile_df = percentile_df)){
          
          #if check is positive, then flag is changed
          #identify which positions need to be flagged
          frequent_value_flag[i+which(x[i:(i+9),'Precip'] == as.numeric(names(val_rep))) -1] <- TRUE
          
        }
        
      } else{
        #case that we have two times repitions of 5
        sus_val_present <- as.numeric(names(table(x[i:(i+9),'Precip']))) >= as.numeric(percentile_df[weather$doy[i],"90%"])
        
        #in case the check returned NA, change it to FALSE instead
        sus_val_present[is.na(sus_val_present)] <- FALSE
        
        #at least one exceeds the test
        if(any(sus_val_present)){
          #identify which ones exceed the test
          sus_val <- as.numeric(names(table(x[i:(i+9),'Precip'])[sus_val_present]))
          #identify position of values in x
          sus_position <- i+which(x[i:(i+9),'Precip'] %in% sus_val)-1
          #change flag accordingly
          frequent_value_flag[sus_position] <- TRUE
        }
      }
      
    }
    
  }
  
  #need to bring frequent value flag to same size as weather again
  
  #bind flag to x
  x$flag <- frequent_value_flag
  
  #merge x and weather
  weather <- merge(weather, x, by = colnames(weather), all.x = T) %>%
    arrange(Date)
  
  #change NAs in Flag to FALSE
  weather$flag[is.na(weather$flag)] <- FALSE
  
  #return flag
  return(weather$flag)
  
}


#####
#outlier checks
#####

#helper function to detect median position
which.quantile <- function (x, probs, na.rm = FALSE){
  if (! na.rm & any (is.na (x)))
    return (rep (NA_integer_, length (probs)))
  
  o <- order (x)
  n <- sum (! is.na (x))
  o <- o [seq_len (n)]
  
  nppm <- n * probs - 0.5
  j <- floor(nppm)
  h <- ifelse((nppm == j) & ((j%%2L) == 0L), 0, 1)
  j <- j + h
  
  j [j == 0] <- 1
  o[j]
}

#helper function to detect gaps in monthly split data
get_gap_monthly <- function(x, temp =TRUE, gap_threshold){
  
  gap_flag <- rep(FALSE, length(x))
  
  #step to take for the iterations
  step <- 1
  
  if(temp == TRUE){
    #detect position of median. make this the new start for the search
    start <- which.quantile(x,probs = 0.5)
    
    #increment for next iteration, in case of temperature go up and go down
    step <- c(1,-1)
    
  } else{
    #in case of precipitation, start at first non-zero value
    start <- min(which(x > 0))
  }
  
  for(inc in step){
    gap_found <- FALSE
    i <- start
    
    while(gap_found == FALSE){
      
      i <- i + inc
      
      #once the subsript gets out of the valid options, stop the algorithm
      if(i %in% c(0, length(x))){
        break()
      }
      
      if(diff(x)[i] >= gap_threshold){
        gap_found <- TRUE
      }
    } #end of while loop
    
    #in case a gap was found, adjust the flag vector
    if(gap_found == TRUE){
      #if inc is positive, set everything above as outlier
      if(inc == 1){
        gap_flag[(i+1):length(x)] <- TRUE
      } else{
        gap_flag[1:i] <- TRUE
      } 
    }
    
    
    
  }#end of for loop
  

  return(gap_flag)
}


#gap check

perform_gap_check <- function(weather, var, temp_gap_threshold = 10, 
                              prec_gap_thrshold = 300){

  #set the threshold for the variable accordingly
  if(var %in% c('Tmin', 'Tmax')){
    gap_threshold <- temp_gap_threshold
  } else if(var == 'Precip'){
    gap_threshold <- prec_gap_thrshold
  }

  #split data per month
  var_per_month <-  split(weather, weather$Month)
  
  #perform the search for gaps on the monthly split data
  gap_flag <- map(var_per_month, function(x){
    #drop na values
    x <- x[is.na(x[,var]) == FALSE,]
    
    #sort decreasing 
    x <- arrange(x, x[,var])
    
    #get monthly flag
    x$gap_flag <-  get_gap_monthly(x = x[[var]], gap_threshold = gap_threshold)
    
    return(x)
    
  }) %>%
    bind_rows() %>%
    arrange(Date) %>%
    merge.data.frame(weather, ., by = colnames(weather), all.x = TRUE) %>%
    select(gap_flag)
  
  #replace NAs with FALSE values
  gap_flag <- gap_flag[,1] %>%
    replace_na(FALSE)

  #return gap_flag column
  return(gap_flag)


}

#get long term mean and sd of each doy for a window of 15days centered at day of interest
get_longterm_mean_and_sd <- function(weather, var, doy){
  #get doys of target days
  lim_doy <- doy + c(-7,7)
  
  #adjust for lower lims in old year
  lim_doy <- ifelse(lim_doy <= 0, yes = lim_doy + 365,no = lim_doy)
  
  #adjsut for upper lims in new year
  lim_doy <- ifelse(lim_doy >= 365, yes = lim_doy - 365, no = lim_doy)
  
  
  #if the range of days falls inbetween two years, than different subsetting needed
  if(lim_doy[1] > lim_doy[2]){
    
    target_days <- weather$doy >= lim_doy[1] | weather$doy <= lim_doy[2]
    
  } else {
    
    target_days <- weather$doy >= lim_doy[1] & weather$doy <= lim_doy[2]
  }
  

  
  return(data.frame(doy = doy,
                    mean = mean(weather[[var]][target_days], na.rm =T), 
                    sd = sd(weather[[var]][target_days], na.rm =T)))

}

perform_climate_outlier_check <- function(weather, var, max_temperature_z = 6, 
                                          max_prec_threshold = 9, 
                                          max_prec_threshold_freezing = 5,
                                          prec_percentile = 0.95){
  
  if(var %in% c('Tmin', 'Tmax')){
    
    #calculate longt term mean and sd of temperature for each day of the year for a 15 day window centered at day of interest
    clim_df <- map(unique(weather$doy), ~ get_longterm_mean_and_sd(weather = weather, var = var, doy = .x)) %>%
      bind_rows()
    
    #normalise temperature data
    
    weather <- merge(weather, clim_df, by = 'doy') %>%
      arrange(Date)
    
    clim_outlier <- abs((weather[,var] - weather$mean) / weather$sd) > max_temperature_z
      
    clim_outlier <- replace_na(data = clim_outlier, replace = FALSE)
    
    return(clim_outlier)
  } else if(var == 'Precip'){
    
    weather <- map(unique(weather$doy), ~ get_clim_percentiles_prec(weather = weather, 
                                                         doy = .x, probs = prec_percentile)) %>%
      unlist() %>%
      data.frame(doy = unique(weather$doy), percentile = .) %>%
      merge.data.frame(weather, ., by = 'doy') %>%
      arrange(Date)
    
    #in case precipitation happening at freezing temperatures, choose a lower threshold
    clim_outlier <- weather[,var] >= ifelse((weather$Tmax + weather$Tmin) / 2 > 0, yes = weather$percentile * max_prec_threshold, 
           no = weather$percentile * max_prec_threshold_freezing)
    
    #change na to false
    clim_outlier <- replace_na(data = clim_outlier, replace = FALSE)
    
    return(clim_outlier)
  }
}


####temporal consistency checks

#iterative temperature consistency

weather <- weather_list[[1]]

quickker_iterat_consistency <- function(weather){
  
  #this objects determines how long the while loop goes, start value is arbetrary and just chosen, so that the while loop runs at least one time
  max_violations <- 2
  
  #object to save the flags
  tmin_flag <- tmax_flag <- tmean_flag <- rep(FALSE, nrow(weather))
  
  while(max_violations > 1){
    #check which observations are available
    tmin0 <- is.na(weather$Tmin) == FALSE
    tmin1 <- is.na(tmin_lead) == FALSE
    tmax0 <- is.na(weather$Tmax) == FALSE
    tmax1 <- is.na(tmax_lead) == FALSE
    tmean0 <- is.na(weather$Tmean) == FALSE
    
    #objects to count the amount of positive tests 
    tmin_violations <- tmax_violations <- tmean_violations <- rep(0, nrow(weather))
    
    #1
    t1 <- ifelse(tmax0 & tmin0, yes = weather$Tmax < (weather$Tmin-1), no = FALSE)
    
    #2
    t2 <- ifelse(tmean0 & tmax0, yes = ifelse(weather$Tmean > (weather$Tmax + 1), yes = TRUE, no = FALSE), no = FALSE)
    
    #3
    t3 <- ifelse(tmean0 & tmin0, yes = weather$Tmean < (weather$Tmin + 1), no = FALSE)
    
    #4
    t4 <- ifelse(tmax0 & tmin1, yes = (weather$Tmax < (lead(weather$Tmin) - 1)), no = FALSE)
    
    #5
    t5 <- ifelse(tmin0 & tmax1, yes = weather$Tmin > (lead(weather$Tmax) + 1), no = FALSE)
    
    #6
    t6 <- ifelse(tmax1 & tmean0, yes = lead(weather$Tmax) < (weather$Tmin -1), no = FALSE)
    
    #7
    t7 <- ifelse(tmin1 & tmean0, yes = lead(weather$Tmin) > (weather$Tmean +1), no = FALSE)
    
    #count the amount of positive tests per reading, account for lead values
    tmax_violations <- t1 + t2 + t4 + lag(t5) + lag(t6)
    tmin_violations <- t1 + t3 + t5 + lag(t4) + lag(t7)
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

perform_iterative_temperature_consistency <- function(weather){
  
  #flag which is later returned
  tmean_flag <- tmin_flag <- tmax_flag <- rep(FALSE, nrow(weather))
  
  #object that counts the violations
  tmin_violations <- tmax_violations <- tmean_violations <- rep(0, nrow(weather))
  
  #objects used in loop
  tmin0 <- tmin1 <- tmax0 <- tmax1 <- tmean0 <- tmean1 <- TRUE
  
  #this criterion is used to control the length of the while loop
  #first value just to start while loop
  max_violation <- 2
  
  while(max_violation > 1){
    
    #reset object that counts the violations
    tmin_violations <- tmax_violations <- tmean_violations <- rep(0, nrow(weather))
    
    #iterate over each day, count the violations
    for(i in 1:(nrow(weather)- 1)){
      
      #check which observations are available
      tmin0 <- is.na(weather[i, 'Tmin']) == FALSE
      tmin1 <- is.na(weather[i+1, 'Tmin']) == FALSE
      tmax0 <- is.na(weather[i, 'Tmax']) == FALSE
      tmax1 <- is.na(weather[i+1, 'Tmax']) == FALSE
      tmean0 <- is.na(weather[i, 'Tmean']) == FALSE
      tmean1 <- is.na(weather[i+1, 'Tmean']) == FALSE
      
      #1
      if(tmax0 & tmin0){
        if(weather[i, 'Tmax'] < (weather[i, 'Tmin'] - 1)){
          tmin_violations[i] <- tmin_violations[i] + 1
          tmax_violations[i] <- tmax_violations[i] + 1
        }
      }
      
      #2
      if(tmean0 & tmax0){
        if(weather[i, 'Tmean'] > (weather[i, 'Tmax'] + 1)){
          tmean_violations[i] <- tmean_violations[i] + 1
          tmax_violations[i] <- tmax_violations[i] + 1
        }
      }
      
      #3
      if(tmean0 & tmin0){
        if(weather[i, 'Tmean'] < (weather[i, 'Tmin'] - 1)){
          tmean_violations[i] <- tmean_violations[i] + 1
          tmin_violations[i] <- tmin_violations[i] + 1
        }
      }
      
      #4
      if(tmax0 & tmin1){
        if(weather[i, 'Tmax'] < (weather[i+1, 'Tmin'] -1)){
          tmin_violations[i+1] <- tmin_violations[i+1] + 1
          tmax_violations[i] <- tmax_violations[i] + 1
        }
      }
      
      #5
      if(tmin0 & tmax1){
        if(weather[i, 'Tmin'] > (weather[i+1, 'Tmax'] + 1)){
          tmin_violations[i] <- tmin_violations[i] + 1
          tmax_violations[i+1] <- tmax_violations[i+1] + 1
        }
      }
      
      #6
      if(tmax1 & tmean0){
        if(weather[i+1, 'Tmax'] < (weather[i, 'Tmean'] - 1)){
          tmean_violations[i] <- tmean_violations[i] + 1
          tmax_violations[i+1] <- tmax_violations[i+1] + 1
        }
      }
      
      #7
      if(tmin1 & tmean0){
        if(weather[i+1, 'Tmin'] > (weather[i, 'Tmean'] + 1)){
          tmin_violations[i+1] <- tmin_violations[i+1] + 1
          tmean_violations[i] <- tmean_violations[i] + 1
        }
      }
    }
    max_violation <- max(tmin_violations, tmax_violations, tmean_violations)
    
    if(max_violation >= 1){
      #mark which ones violated the most in flag
      tmin_flag[tmin_violations == max_violation] <- TRUE
      tmax_flag[tmax_violations == max_violation] <- TRUE
      tmean_flag[tmean_violations == max_violation] <- TRUE
      
      #set the valuzes which accumulated most violations to NA
      weather[tmin_violations == max_violation, 'Tmin'] <- NA
      weather[tmax_violations == max_violation, 'Tmax'] <- NA
      weather[tmean_violations == max_violation, 'Tmean'] <- NA
    }
    
    
  }
  
  #additionally_flag any remaining days for which Tmin > Tmax
  tmin_flag[weather$Tmin > weather$Tmax] <- TRUE
  tmax_flag[weather$Tmin > weather$Tmax] <- TRUE
  
  return(data.frame('Tmin_flag' = tmin_flag, 'Tmean_flag' = tmean_flag, 
                    'Tmax_flag' = tmax_flag))
}

#spike and dip check
#temperature day at 0 is larger than +-25 than day -1 and 1

do_spike_dip_test <- function(weather, var, dip_threshold = 25){
  flag <- abs(weather[,var] - lead(weather[,var])) >= dip_threshold & abs(weather[,var] - lag(weather[,var])) >= dip_threshold
  
  flag <- replace_na(flag[,1], FALSE)
  return(flag)
}

#lagged range test
perform_lagged_temperature_check <- function(weather, max_diff = 40){
  #get lowesr tmax for each day using a trhee day window
  lowest_tmax <- apply(matrix(c(lag(weather$Tmax), weather$Tmax, lead(weather$Tmax)),nrow = nrow(weather),
                              ncol = 3, byrow = FALSE), MARGIN = 1, min, na.rm = T)
  
  #cases with only NA return Inf, change to NA
  lowest_tmax[is.infinite(lowest_tmax)] <- NA
  
  #create flag for tmin and tmax. each day of tmin tested true gets flagged, aswell as the tree day windows of tmax
  tmin_flag <- tmax_flag <- weather$Tmin <= lowest_tmax - max_diff
  addtional_true <- c(which(tmax_flag) + 1, which(tmax_flag) - 1) %>%
    .[. != 0 | .!= length(tmin_flag)]
  tmax_flag[addtional_true] <- TRUE
  
  #same for tmax
  #get lowesr tmax for each day using a trhee day window
  highest_tmin <- apply(matrix(c(lag(weather$Tmin), weather$Tmin, lead(weather$Tmin)),nrow = nrow(weather),
                               ncol = 3, byrow = FALSE), MARGIN = 1, min, na.rm = T)
  
  #cases with only NA return Inf, change to NA
  highest_tmin[is.infinite(highest_tmin)] <- NA
  
  #create flag for tmin and tmax. each day of tmin tested true gets flagged, aswell as the tree day windows of tmax
  tmin_flag2 <- tmax_flag2 <- weather$Tmax >= highest_tmin + max_diff
  addtional_true <- c(which(tmax_flag2) + 1, which(tmax_flag2) - 1) %>%
    .[. != 0 | .!= length(tmin_flag)]
  tmax_flag2[addtional_true] <- TRUE
  
  #return any case of tmin_flag / tmin_flag2; tmax_flag | tmax_flag2
  return(data.frame('Tmin_flag' = (tmin_flag | tmin_flag2), 
                    'Tmax_flag' = (tmax_flag | tmax_flag2)))
}

###
#temperature corrobation check

perform_temperature_corrobation_check <- function(weather, weather_coords,
                                                  aux_list, aux_info,
                                                  max_station = 7, min_station = 3,
                                                  max_dist = 75, max_diff = 10){
  #get climate mean and sd for each day of target station
  weather <- map(unique(weather$doy), ~get_longterm_mean_and_sd(weather = weather, var = 'Tmin',
                                                                doy = .x)) %>%
    bind_rows() %>%
    merge(weather, by = 'doy', all.y = TRUE) %>%
    arrange(Date)
  
  #calculate climate anomaly
  weather$anomaly <- (weather[,var] - weather$mean) / weather$sd
  
  #calculate temperature anomalies for a subset of closest stations of a three day window
  #take anomaly closest to target anomaly
  #if difference is greater than 10°C, then target value is flagged
  
  #this should be done ONCE, it happens also in the temperature spatial consistency test
  
  #calculate distance to aux_stations
  aux_info$dist <-  round(sp::spDistsN1(pts = as.matrix(aux_info[, c("Longitude", "Latitude")]),
                                        pt = weather_coords, longlat = TRUE), 2)
  
  #select stations within the max distance, which are not the target station
  aux_info <- aux_info %>%
    filter(dist > 0 & dist <= max_dist) %>%
    arrange(dist)
  
  #if too few neighbouring values, then the test can't be carried out
  if(nrow(aux_info) < min_station){
    return(rep(NA, nrow(weather)))
  }  else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  } 
  aux_list <- aux_list[aux_info$id]
  
  aux_list <- map(aux_list, function(x){
    
    #add date and doy to aux data
    x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d")
    x$doy <- lubridate::yday(x$Date)
    #calculate climate mean and sd of aux data
    climate_df <- map(unique(x$doy), ~get_longterm_mean_and_sd(weather = x, var = var,
                                                               doy = .x)) %>%
      bind_rows() %>%
      merge(x, by = 'doy', all.y = TRUE) %>%
      arrange(Date)
    
    #calculate climate anomaly of aux data
    return((x[,var] - climate_df$mean) / climate_df$sd)
  }) %>%
    map2(., aux_list, function(x,y) tibble(y, anomaly = x))
  
  #check if the absolute min distance of temperatre anomalies exceeds the threshold
  flag <- imap_dbl(weather$Date,~ get_abs_min_difference(weather = weather, i = .y, var = 'anomaly', 
                                                         aux_list = aux_list)) >= max_diff
  return(flag)
}

#naught check: check for wrong zeros (of either degree Fahrenheit or degree Celsius)
perform_naught_check <- function(weather){
  flag <- ifelse((weather$Tmin == -17.8 & weather$Tmax == -17.8) | (weather$Tmin == 0 & weather$Tmax == 0),
         yes = TRUE, no = FALSE)
  
  #change nas to false
  flag[is.na(flag) == TRUE] <- FALSE
  return(flag)
}


#helper function: clear flagged values, mark which test lead to the removel
clear_flagged_data <- function(weather, var, test_result, test_name){
  #set values to NA for positive test results
  weather[test_result, var] <- NA
  
  #indicate which test lead to removal
  weather[test_result, paste0('org_', var)] <- test_name
  
  return(weather)
}

#helper function to carry out the climate percentile check for all doys
get_each_day_precipitation_percentile <- function(weather, probs = c(.3, .5, .7, .9)){
  
  names <- c('doy', paste0(probs * 100, '%'))
  
  map(unique(weather$doy), ~ get_clim_percentiles_prec(weather = weather, 
                                                       doy = .x,
                                                       probs = probs)) %>%
    do.call(rbind, .) %>%
    data.frame(doy = unique(weather$doy), .) %>%
    `colnames<-`(names)
}

region <- 'USA'
sub_region <- 'California'

durre_weather_quality_control <- function(weather_list, weather_info){
  
  #add a column to weather_list objects indicating which test performed positive
  #also add Date and doy
  weather_list <- map(weather_list, function(x) tibble(x, 'Date' = as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d"),
                                                       'Tmin_org' = x$Tmin, 
                                                       'Tmax_org' = x$Tmax, 'Precip_org' = x$Precip,
                                                       'Tmin_flag' = NA, 'Tmax_flag' = NA, 
                                                       'Precip_flag' = NA) %>%
                        mutate(doy = lubridate::yday(Date)))
  
  ####
  #basic integrity check
  ####

  #1: naught check
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, 
                                           var = 'Tmin', 
                                           test_result = perform_naught_check(x),
                                           test_name = 'naught_check'))
  
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, 
                                                           var = 'Tmax', 
                                                           test_result = perform_naught_check(x),
                                                           test_name = 'naught_check'))
    
  #2 duplicate check
  #between entire years only for precipitation
  #tmin
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, var = 'Tmin', 
                                           test_result = get_duplicated_values(weather = x, var = 'Tmin'), 
                                           test_name = 'duplicated'))
  
  #tmax
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, var = 'Tmax', 
                                                           test_result = get_duplicated_values(weather = x, var = 'Tmax'), 
                                                           test_name = 'duplicated'))
  #precipitation
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, var = 'Precip', 
                                                           test_result = get_duplicated_values(weather = x, var = 'Precip'), 
                                                           test_name = 'duplicated'))
  
  ###Problem: flags also months with only NAs (this doesn't make sense)
  #           flags month with sparse precipitation, especially completely dry months
  
  #### record exceedance test
  
  #download records
  records <- get_temp_records(region = region) %>%
    filter(Country == sub_region)
  
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmin', 
                       test_result = fixed_limit_test(weather = x, 
                                                      var = 'Tmin',
                                                      level = NULL,
                                                      country = NULL,
                                                      records = c(records$Tmin, records$Tmax)), 
                       test_name = 'record_exceedance')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmax', 
                       test_result = fixed_limit_test(weather = x, 
                                                      var = 'Tmax',
                                                      level = NULL,
                                                      country = NULL,
                                                      records = c(records$Tmin, records$Tmax)), 
                       test_name = 'record_exceedance')
  })
  
  #Precipitation
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Precip', 
                       test_result = fixed_limit_test(weather = x, 
                                                      var = 'Precip',
                                                      level = NULL,
                                                      country = NULL,
                                                      records = c(0, records$Precip)), 
                       test_name = 'record_exceedance')
  })
  
  
  #identical value streak test
  
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmin', 
                       test_result = get_streaks(weather = x, 
                                                      var = 'Tmin'), 
                       test_name = 'streaks')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmax', 
                       test_result = get_streaks(weather = x, 
                                                 var = 'Tmax'), 
                       test_name = 'streaks')
  })
  
  #Precip
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Precip', 
                       test_result = get_streaks(weather = x, 
                                                 var = 'Precip'), 
                       test_name = 'streaks')
  })
  
  #calculate percentiles for each weather df, store in list
  prec_percentile_list <- map(weather_list, get_each_day_precipitation_percentile)
  
  
  weather_list <- map2(weather_list, prec_percentile_list, function(x,y){
    clear_flagged_data(weather = x, var = 'Precip', 
                       test_result = frequent_value_check(weather = x, 
                                                          percentile_df = y),
                       test_name = 'frequent_ident_value')
  })
  
  
  ####
  #outlier checks
  ####
  
  #gap check
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmin', 
                       test_result = perform_gap_check(weather = x, 
                                                 var = 'Tmin'), 
                       test_name = 'gap_check')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmax', 
                       test_result = perform_gap_check(weather = x, 
                                                       var = 'Tmax'), 
                       test_name = 'gap_check')
  })
  
  #Precip
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Precip', 
                       test_result = perform_gap_check(weather = x, 
                                                       var = 'Precip'), 
                       test_name = 'gap_check')
  })
  

  #climatological outlier
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmin', 
                       test_result = perform_climate_outlier_check(weather = x, 
                                                       var = 'Tmin'), 
                       test_name = 'clim_outlier')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmax', 
                       test_result = perform_climate_outlier_check(weather = x, 
                                                                   var = 'Tmax'), 
                       test_name = 'clim_outlier')
  })
  
  #Precip
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Precip', 
                       test_result = perform_climate_outlier_check(weather = x, 
                                                                   var = 'Precip'), 
                       test_name = 'clim_outlier')
  })
  
  
  ######
  #Temporal consistency checks
  ######
  
  #iterative temperature consistency
  
  test_list <- map(weather_list, quickker_iterat_consistency)
  
  weather_list <- map2(weather_list, test_list, function(x,y){
    clear_flagged_data(weather = x, var = 'Tmin', test_result = y$tmin_flag, 
                       test_name = 'iterative_consistency')
  })
  
  weather_list <- map2(weather_list, test_list, function(x,y){
    clear_flagged_data(weather = x, var = 'Tmax', test_result = y$tmax_flag, 
                       test_name = 'iterative_consistency')
  })
 
  #spike - dip check
  #tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmin', 
                       test_result = do_spike_dip_test(weather = x, 
                                                                   var = 'Tmin'), 
                       test_name = 'spike-dip')
  })
  
  #tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, var = 'Tmax', 
                       test_result = do_spike_dip_test(weather = x, 
                                                       var = 'Tmax'), 
                       test_name = 'spike-dip')
  })

  
  #lagged temperature range check
  
  
  
  #####
  #spatial consistency check
  #####
  
  #regression check
  
  #corrobation check: temperature
  
  #corrobation check: precipitation
  
  
  ####
  #mega consistency check
  ####
  
  #extremes mega consistency check
  
  
}


#now Ive got all the functions of durre (at least the ones which includes tmin, tmax and precip; snow and tobs were mostly ignored)
#put everything together
#order of tests is important, also each station (also in aux_list) should be applied to the same funciton, before going to the next test
#add functionality which removes flagged values from the dataframe



