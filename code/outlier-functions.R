#scrap temperature records from wikipedia
#follow example of https://towardsdatascience.com/scraping-data-from-wikipedia-tables-3efa04c6b53f
library(tidyverse)
#library(progress)
options(dplyr.summarise.inform = FALSE)

#function to get temperature records
get_weather_records <- function(region = 'world'){
  
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
    
    #cases where we have negative value: strip the weird minus sign and add the 'real' minus
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
    records <- data.table::fread('https://www.ncdc.noaa.gov/extremes/scec/records.csv',
                                 showProgress = FALSE)
    
    #take min and max temperature, change from fahrenheit to degree celsius
    records <- records %>%
      dplyr::select(State, Element, Value) %>%
      filter(Element %in% c('All-Time Maximum Temperature', 'All-Time Minimum Temperature', 'All-Time Greatest 24-Hour Precipitation')) %>%
      mutate(Element = as.factor(Element)) %>%
      mutate(Element = recode_factor(Element, 'All-Time Minimum Temperature' = 'Tmin', 
                                     'All-Time Maximum Temperature' = 'Tmax',
                                     'All-Time Greatest 24-Hour Precipitation' = 'Precip')) %>%
      .[!duplicated(.),] %>%
      reshape2::dcast(State ~ Element, value.variable = 'Value', value.var = 'Value') %>%
      mutate(Tmin = round((as.numeric(Tmin) - 32) * (5/9), digits = 1),
             Tmax = round((as.numeric(Tmax) - 32) * (5/9), digits = 1),
             Precip = Precip * 25.4)
    

    records <- as_tibble(records)
  }
  
  names(records) <- c('Country', 'Tmin', 'Tmax', 'Precip')
  return(records)
}



###fixed limits test

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


###variable limit test

test_variable_limit <- function(weather, variable, probs = c(0.01, 0.99)){
  
  #split to monthly groups
  monthly_weather <- split(weather, f = weather$Month)
  
  
  monthly_test <- weather %>%
    split(f = .$Month) %>%
    purrr::map(function(x) quantile(x[,variable], probs = probs, na.rm = T)) %>%
    purrr::map2(monthly_weather, function(x,y) ifelse(is.na(y[,variable]), yes = FALSE, no = y[,variable] < x[1] | y[,variable] > x[2])) %>%
    unsplit(f = weather$Month)
  

  #also test for yearly quantiles
  yearly_test <- quantile(weather[,variable], probs = c(probs[1], probs[2]), na.rm = T) %>%
    {ifelse(is.na(weather[,variable]), yes = F, no = ((weather[, variable] < .[1])|(weather[,variable]>.[2])))}
  
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

test_temporal_continuity <-  function(weather, variable, prob = 0.995){
  #calculate difference to next day, append one NA to keep same length
  diffs <- c(abs(diff(as.matrix(weather[,variable]))), NA)
  
  #get the quantile of the jumps, everything above the quantile is labelled as suspicious
  jump_quan <- quantile(diffs, probs = prob, na.rm = T)
  
  return(ifelse(is.na(diffs), yes = F, no = (diffs > jump_quan)))
  
}


#### consitstency between variables 
test_temperature_consistency <- function(weather, probs = 0.99){
  #minimum temperature should not be higher then maximum temperature (also one day earlier or later)
  cons1 <- (weather$Tmin >= weather$Tmax) | (weather$Tmin >= dplyr::lag(weather$Tmax)) | (weather$Tmin >= dplyr::lead(weather$Tmax))

  #min temperature should be lower than tmean
  cons2 <- (weather$Tmin >= weather$Tmean) | (weather$Tmin >= dplyr::lag(weather$Tmean)) | (weather$Tmin >= dplyr::lead(weather$Tmean))
  
  #tmean should not be too different from (tmin + tmax)/2
  diff_tmean <- abs(((weather$Tmin + weather$Tmax)/2) - weather$Tmean)
  perc_diff_tmean <- quantile(diff_tmean, prob = probs, na.rm = T)
  cons3 <- diff_tmean > perc_diff_tmean
  
  
  return(ifelse(test = is.na(cons1 | cons2 | cons3), yes = F, no = (cons1 | cons2 | cons3)))
  
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

#add date to auxlist
aux_data <- map(aux_data, function(x){
  x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = '%Y-%m-%d')
  x
})



#get percentile of precipitation data
get_prec_rank <- function(weather,min_non_zero_days = 20){
  
  #add date and doy
  weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep ='-'),
                          format = '%Y-%m-%d')
  weather$doy <- lubridate::yday(weather$Date)
  
  
  
  ecdf_list <- map(unique(weather$doy), ~ get_ecdf(weather = weather, doy = .x, min_non_zero_days = min_non_zero_days))
  

  #somehow this only works if I assign one of the functions to y
  y <- ecdf_list[[1]]

  return(map2(split(weather,f = weather$doy), ecdf_list, function(x,y) floor(sapply(x$Precip,y) * 100)) %>%
           unsplit(weather$doy))
  
}

#there is a problem with this function! weather and aux stations can
#have different length, so using index isnt advicable
#use date for subsetting instead

#get absolute minimum difference either for precipitation or precipitation percentile rank
get_abs_min_difference <- function(x, target_date, variable, aux_list){
  
  if(is.na(x) == TRUE){
    return(NA)
  }
  
  
  #extract values from aux station with +-1 day
  int <- map(aux_list, function(x){
    x %>%
      filter(Date >= (target_date - 1) & Date <= (target_date + 1)) %>%
      .[[variable]]
  }) %>%
    unlist() %>%
    unname()
  
  #check if there is at least one value of non NA. otherwise return NA,
  #in case no neighbouring station covered the period of interest also return NA
  if(length(int) > 0){
    if(all(is.na(int))){
      return(NA)
    }
  } else{
    return(NA)
  }
  
  
  #if the value is not the highest or the lowest, then there is no need to carry out 
  #the corrobation test
  
  if(variable == 'Precip'){
    #in case x isn't the largest or the smallest, return zero
    if(all(x > int, na.rm = T) | all(x < int, na.rm = T) == F){
      return(0)
    } else{
      #otherwise calculate absolute minimum difference
      return(min(abs(x - int), na.rm =T))
    }
  } else{
    return(min(abs(x - int), na.rm = T))
  }
}


test_precipitation_spatial_corrobation <- function(weather, weather_coords, aux_info, aux_list,
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
    return(rep(FALSE, nrow(weather)))
  }  else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  } 
  aux_list <- aux_list[aux_info$id]
  
  ####
  #calculate prec rank
  ####
  
  #calculate for each precipitation value the percentile rank
  weather$prec_rank <- get_prec_rank(weather = weather)
  
  #also add prec rank to aux data
  aux_list <-  map(aux_list, get_prec_rank) %>%
    map2(., aux_list, function(x,y) tibble(y, prec_rank = x))
  
  ####
  #get absolute minimum difference
  ####
  
  #now determine the prec rank difference  
  prec_min_difference <- map2_dbl(weather$Precip, weather$Date, function(x,y) get_abs_min_difference(x = x, target_date = y, variable = 'Precip', aux_list = aux_list))
  #same for precipitation percentile rank
  prec_rank_difference <-  map2_dbl(weather$prec_rank, weather$Date, function(x,y) get_abs_min_difference(x = x, target_date = y, variable = 'prec_rank', aux_list = aux_list))
  
  
  ###
  #get test threshold & and test
  ###
  test_threshold <- (-45.72 * log(prec_rank_difference) + 269.24)
  
  return(replace_na(prec_min_difference > test_threshold, replace = FALSE))
  
}


select_target_days <- function(df, variable, period_start, period_end){
  
  #if not present, add Date column
  df$Date <- as.Date(paste(df$Year, df$Month, df$Day, sep = '-'), format = '%Y-%m-%d')
  
  #in case every day of the target period is also present in the weather dataframe, then simply reutrn everything
  if(all(period_start:period_end %in% df$Date)){
    return(pull(df[df$Date >= period_start & df$Date <= period_end,variable]))
  } else{
    #make a placeholder for the selected days
    re_vec <- rep(NA, as.numeric(period_end-period_start +1))
    
    #change days which are present in the 
    re_vec[period_start:period_end %in% df$Date] <- pull(df[df$Date >= period_start & df$Date <= period_end,variable])
    
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
spat_consist_one_period <- function(weather, aux_list, aux_info, period_start, variable,
                                    max_res = 8, max_res_norm = 4, min_station = 3,
                                    max_station = 7, window_width = 15, 
                                    min_correlation = 0.8, min_coverage = 40){
  

  #add window width to the period
  period_end <- lubridate::ceiling_date(period_start,unit = 'month') + window_width -1
  period_start <- period_start - window_width
  
  #extract data from target (x) and aux (y)
  x <- select_target_days(df = weather, variable = variable, period_start = period_start, period_end = period_end)
  y <- purrr::map(aux_list, function(x){
    select_target_days(df = x, variable = variable, period_start = period_start, 
                       period_end = period_end)
  }) %>%
    do.call(cbind.data.frame, .)
  
  #only keep aux stations which fulfill coverage criteria
  aux_info <- aux_info[colSums(is.na(x) == F & is.na(y) == F) >= min_coverage, ]
  
  #drop stations from y
  y <- y[,aux_info$id]
  
  
  #case there are not enough stations left: return nas as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  }
  
  #calcualte index of agreement and sort decreasing
  aux_info$ind_agreement <- purrr::map_dbl(y, ~ calc_index_agreement(x = x, y = .x))
  aux_info <- aux_info[order(aux_info$ind_agreement,decreasing = T),]
  
  #bring y in same order
  y <- y[,aux_info$id]
  
  #iterate over all y columns, for each column iterate over x and find the closest y value given a 3 day window centered around i
  y_closest <-  purrr::map(y, function(vec) purrr::imap_dbl(x,~get_closest_y(x = .x, y=vec, i = .y))) %>%
    do.call(cbind.data.frame, .)
  
  #carry out linear regression
  models <- purrr::map(y_closest, ~ lm(x~.x))
  
    
  #calculate correlation coefficient, filter stations with too low correlation coefficient
  #keep only stations fulfillinf the criteria of minimum correlation of their prediction
  aux_info <- purrr::map_lgl(models, ~sqrt(summary(.x)[['r.squared']]) > min_correlation) %>%
      aux_info[.,]
  

  #if there are less then 3 stations remaining, then return NAs as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  } else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  }
  
  models <-   models[aux_info$id]
  y_closest <- y_closest[,aux_info$id]
  
  helper_func <- function(x,y,z){
    z -as.numeric((x %*% as.matrix(y)) / sum(y))
  }
  
  #calculate weighted model estimates for each day
  #problem: nas are not returned here, I need to match it with x
  x_res <- purrr::map2(models, y_closest, .f = function(x,y){
    x$coefficients[1] +  x$coefficients[2] * y}) %>%
    bind_cols() %>%
    as.matrix() %>%
    helper_func(aux_info$ind_agreement, x) %>%
    round(digits = 2)
  
  rm(models)  

  #standardized residuals (by mean and std)
  x_res_norm <- (x_res - mean(x_res, na.rm = T)) / sd(x_res, na.rm = T)
  
  
  #take only the values for the month
  #--> strip the leading and trailing 15 values
  x_res <- x_res[(window_width+1):(length(x_res) - (window_width))]
  x_res_norm <- x_res_norm[(window_width+1):(length(x_res_norm) - (window_width))]
  
  flag_res <- ifelse(is.na(x_res), yes = F, no = abs(x_res) >= max_res)
  flag_res_norm <-  ifelse(is.na(x_res_norm), yes = F, no = abs(x_res_norm) >= max_res_norm)
  
  flag <- replace_na(flag_res & flag_res_norm, FALSE)
  
  #if either the residuals or the standardized resiudals exceed the threshold, return for that given day a true
  return(flag)
  
}



#function to perform spatial consistency check for one period
spat_consist_one_period_quick <- function(weather, aux_list, aux_info, period_start, variable,
                                    max_res = 8, max_res_norm = 4, min_station = 3,
                                    max_station = 7, window_width = 15, 
                                    min_correlation = 0.8, min_coverage = 40){
  
  
  #add window width to the period
  period_end <- lubridate::ceiling_date(period_start,unit = 'month') + window_width -1
  period_start <- period_start - window_width
  
  #extract data from target (x) and aux (y)
  x <- select_target_days(df = weather, variable = variable, period_start = period_start, period_end = period_end)
  y <- purrr::map(aux_list, function(x){
    select_target_days(df = x, variable = variable, period_start = period_start, 
                       period_end = period_end)
  }) %>%
    do.call(cbind.data.frame, .)
  
  rm(weather, aux_list)
  
  
  
  
  #only keep aux stations which fulfill coverage criteria
  aux_info <- aux_info[colSums(is.na(x) == F & is.na(y) == F) >= min_coverage, ]
  
  #drop stations from y
  y <- y[,aux_info$id]
  
  
  #case there are not enough stations left: return nas as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  }
  
  #calcualte index of agreement and sort decreasing
  
  aux_info %>%
    mutate(ind_agreement = purrr::map_dbl(y, ~ calc_index_agreement(x = x, y = .x))) %>%
    arrange(desc(ind_agreement))
  
  #bring y in same order
  y <- y[,aux_info$id]
  
  #iterate over all y columns, for each column iterate over x and find the closest y value given a 3 day window centered around i
  y_est <- purrr::map(y, function(vec) purrr::imap_dbl(x,~get_closest_y(x = .x, y=vec, i = .y))) %>%
    do.call(cbind.data.frame, .) %>%
    #carry out linear regression, calculate the model estimates
    purrr::map(., ~ lm(x~.x)) %>%
    purrr::imap(function(x,id) x$coefficients[1] +  x$coefficients[2] * y[,id]) %>%
    bind_cols()
  
  #check if correlation coefficient is large enough to keep station, otherwise drop from list
  aux_info <- aux_info[as.logical(cor(cbind(x,y_est), use = 'pairwise.complete.obs')[1,-1] > min_correlation),]
  
  #if there are less then 3 stations remaining, then return NAs as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  } else if(nrow(aux_info) > max_station){
    
    y_est <- y_est[,1:max_station]
  }
  
  #get weighted mean of model prediction, then calculate the residual
  #change NA values of prediction to 0, so that the matrix multiplication
  #still yields a result
  
  test <- y_est %>%
    replace(is.na(.), 0) %>%
    as.matrix(.) %*% as.matrix(aux_info$ind_agreement) / sum(aux_info$ind_agreement) %>%
    tibble(res =(.- x)) %>%
    slice(-c(1:window_width, (n()-window_width+1):n()))%>%
    select(res) 
  
  res_norm <- (test[,1]-  mean(test[,1], na.rm = T) ) / sd(test[,1], na.rm = T)
    
  flag_res <- ifelse(is.na(test[,1]) ,yes = FALSE, no = abs(test[,1]) >= max_res )
  
  #standardized residuals (by mean and std)
  res_norm <- (x_res - mean(x_res, na.rm = T)) / sd(x_res, na.rm = T)
  flag_res_norm <-  ifelse(is.na(x_res_norm), yes = F, no = abs(x_res_norm) >= max_res_norm)
  
  #if either the residuals or the standardized resiudals exceed the threshold, return for that given day a true
  return(flag_res & flag_res_norm)
  
}

spat_consist_one_period_old <- function(weather, aux_list, aux_info, period_start, variable,
                                    max_res = 8, max_res_norm = 4, min_station = 3,
                                    max_station = 7, window_width = 15, 
                                    min_correlation = 0.8, min_coverage = 40){
  
  
  
  #add window width to the period
  period_end <- lubridate::ceiling_date(period_start,unit = 'month') + window_width -1
  period_start <- period_start - window_width
  
  #extract data from target (x) and aux (y)
  x <- select_target_days(df = weather, variable = variable, period_start = period_start, period_end = period_end)
  y <- purrr::map(aux_list, ~ select_target_days(df = .x, variable = variable, period_start = period_start, period_end = period_end)) %>%
    do.call(cbind.data.frame, .)
  
  #remove not needed objects
  rm(weather, aux_list)
  
  #only keep aux stations which fulfill coverage criteria
  aux_info <- aux_info[colSums(is.na(x) == F & is.na(y) == F) >= min_coverage, ]
  
  #drop stations from y
  y <- y[,aux_info$id]
  
  #if there are less then 3 stations remaining, then return NAs as flag
  if(nrow(aux_info) < min_station){
    return(rep(NA, (period_end - window_width) - (period_start+window_width) + 1))
    
    #return only nas
  }
  
  
  
  
  #calcualte index of agreement and sort decreasing
  aux_info$ind_agreement <- purrr::map_dbl(y, ~ calc_index_agreement(x = x, y = .x))
  aux_info <- aux_info[order(aux_info$ind_agreement,decreasing = T),]
  
  #bring y in same order
  y <- y[,aux_info$id]
  
  #iterate over all y columns, for each column iterate over x and find the closest y value given a 3 day window centered around i
  y_closest <-  purrr::map(y, function(vec) purrr::imap_dbl(x,~get_closest_y(x = .x, y=vec, i = .y))) %>%
    do.call(cbind.data.frame, .)
  
  rm(y)
  
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


#function for spatial consistency test, extract target period from weather data
#function prevents that if subscript is out of bounds that null is returned, instead ensures same length for all stations
test_spatial_consistency <- function(weather, weather_coords, aux_list, aux_info, 
                                     variable, max_dist = 75, window_width = 15, 
                                     min_coverage = 40, min_correlation = 0.8,
                                     min_station = 3, max_station = 7, max_res = 8, 
                                     max_res_norm = 4){
  

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
  spatial_flags <- purrr::map(starts, function(x){
    spat_consist_one_period(weather = weather, aux_list = aux_list,
                             aux_info = aux_info, variable = variable, period_start = x, 
                             window_width = window_width, max_res = max_res, 
                             max_res_norm = max_res_norm, min_station = min_station, 
                             max_station = max_station, min_correlation = min_correlation, 
                             min_coverage = min_coverage)})
  
 
  #chnage nas to false
  spatial_flags <- replace_na(unlist(spatial_flags), FALSE)
  
  #return the the flags in form of a list
  return(spatial_flags)
  
}  


#outlier test combined

weather_qc_costa <- function(weather, weather_coords, variable,
                             aux_list, aux_info, region, subregion, records = NULL,
                             probs_variable_limit = c(0.01, 0.99),
                             probs_temporal_continuity = 0.995,
                             probs_temperature_consistency = 0.99,
                             max_dist = 75, window_width = 15, 
                             min_coverage = 40, min_correlation = 0.8,
                             min_station = 3, max_station = 7, max_res = 8, 
                             max_res_norm = 4){
  
  #if temperature, then Tmean is needed
  if(variable %in% c('Tmin', 'Tmax')){
    if(!'Tmean' %in% colnames(weather)){
      stop('For variable consistency test daily average temperature called "Tmean" is required')
    }
  }
  
  if(!'Date' %in% colnames(weather)){
    weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                            format = '%Y-%m-%d')
  }
  
  #make sure weather and aux_list are tibbles
  weather <- tibble(weather)
  aux_list <- map(aux_list, tibble)
  
  #make sure that weather has date column
  weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                          format = '%Y-%m-%d')

  #do testing of input stuff: 
  #   weather needs to have certain columns
  #   weather coords needs to be of length two and numeric
  #   variable should be tmin, tmax (or preciptiation)
  #   aux_list needs to be list, names need to be same as in aux_info$id; colnames should contain same objects as weather does
  #   aux_info needs to contain coordinates and date?
  
  #call fixed limits test
  fixed_lim <- test_fixed_limit(weather = weather, region = region, 
                   subregion = subregion,variable = variable)
  
  #call variable limits test
  variable_lim <- test_variable_limit(weather = weather, variable = variable,
                                      probs = probs_variable_limit)
  
  #call temporal consistency test
  temporal_consistency <- test_temporal_continuity(weather = weather, variable = variable, 
                                                   prob = probs_temporal_continuity)
  
  #call variable consistency test (only applicable for temperature data)
  #make ifelse condition for temperature and precipitation
  if(variable %in% c('Tmin', 'Tmax')){
    variable_consistency <- test_temperature_consistency(weather = weather,
                                                            probs = probs_temperature_consistency)
    
    spatial_consistency <- test_spatial_consistency(weather = weather, 
                                                    weather_coords = weather_coords, 
                                                    aux_list = aux_list, 
                                                    aux_info = aux_info, 
                                                    variable = variable, 
                                                    max_dist = max_dist, 
                                                    window_width = window_width, 
                                                    min_coverage = min_coverage, 
                                                    min_correlation = min_correlation,
                                                    min_station = min_station, 
                                                    max_station = max_station, 
                                                    max_res = max_res, 
                                                    max_res_norm = max_res_norm)
  } else if(variable == 'Precip'){
    variable_consistency <- NA
    
    spatial_consistency <- test_precipitation_spatial_corrobation(weather = weather, 
                                                                  weather_coords = weather_coords,
                                                                  aux_info = aux_info,
                                                                  aux_list = aux_list,
                                                                  max_dist = max_dist,
                                                                  max_station = max_station,
                                                                  min_station = min_station)
  }
  
  
  
  test_res <- tibble('fixed_limit' = fixed_lim, 
                     'variable_limit' = variable_lim, 
                     'temporal_consistent' =  temporal_consistency, 
                     'consistent_variables' = variable_consistency, 
                     'spatial_consistent' = spatial_consistency)
  
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

#weather <- weather_list[[1]]

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
      group_by(Year) %>%
      summarise(n_nonzero = sum(Precip > 0, na.rm =T)) %>%
      filter(n_nonzero >= 3) %>%
      select(Year) %>%
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
      group_by(Year, Month) %>%
      summarise(n_nonzero = sum(Precip > 0, na.rm =T)) %>%
      filter(n_nonzero >= 3) %>%
      mutate(period = paste(Year, Month, sep = ' ')) %>%
      pull(period)
    
    #subset weather by invest period
    weather <- weather[weather$period %in% invest_periods,]
  }
  

  #check if there is in one year a duplicated month
  t2 <- split(weather, weather$Year) %>%
    map_lgl(function(x) any(duplicated(split(x[,variable], x$Month))))
  
  if(any(t2)){
    #in case there is duplictaed month, find out which year
    sus_year <- split(weather, weather$Year) %>%
      map_lgl(function(x) any(duplicated(split(x[,variable], x$Month)))) %>%
      which() %>%
      unique(weather$Year)[.]
    
    #for this year(s) checj which months are duplciated
    res1 <- weather %>%
      filter(Year %in% sus_year) %>%
      split(.$Year) %>%
      map(function(x) duplicated(split(x[,variable], x$Month))) %>%
      map(which)
    
    res2 <-weather %>%
      filter(Year %in% sus_year) %>%
      split(.$Year) %>%
      map(function(x) duplicated(split(x[,variable], x$Month), fromLast=TRUE)) %>%
      map(which)
    
    #subset for months which yielded duplications
    months <- map2(res1, res2, c) %>%
      map(unique) %>%
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
    map_lgl(function(x) any(duplicated(split(x[,variable], x$Year))))
  
  if(any(t3)){
    #identify month of duplicates
    sus_month <- split(weather, weather$Month) %>%
      map_lgl(function(x) any(duplicated(split(x[,variable], x$Year)))) %>%
      which() %>%
      unique(weather$Month)[.]
    
    #identify which year of the months is problematic
    
    #for this year(s) checj which months are duplciated
    res1 <- weather %>%
      filter(Month %in% sus_month) %>%
      split(.$Month) %>%
      map(function(x) duplicated(split(x[,variable], x$Year))) %>%
      map(which)
    
    res2 <-weather %>%
      filter(Month %in% sus_month) %>%
      split(.$Month) %>%
      map(function(x) duplicated(split(x[,variable], x$Year), fromLast=TRUE)) %>%
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
    flag_dup_mon2 <-  org_weather$period %in% periods
    
  }
  
  
  #####
  #tmin == tmax of at least 10 or more --> flag whole month
  #####
  
  if(variable %in% c('Tmin', 'Tmax')){
    sus_period <- weather %>%
      group_by(Year, Month) %>%
      summarise(same_temp = sum(Tmin == Tmax, na.rm = T)) %>%
      mutate(period = paste(Year, Month, sep = ' ')) %>%
      filter(same_temp >= same_temp_threshold) %>%
      pull(period)
    
    #in case of at least one incident, change the flag
    if(length(sus_period) > 0){
      flag_ident_temp <- org_weather$period %in% sus_period
    }
    
  }
  
  #combine the different flags
  flag <- flag_dup_year | flag_dup_mon | flag_dup_mon2 | flag_ident_temp
  
  #change nas to false
  flag[is.na(flag) == TRUE] <- FALSE
  
  return(flag)
}





###repition checks


get_streaks <- function(weather, variable, rep_threshold = 20){
  
  #if not present, add date to weather
  if(!('Date' %in% colnames(weather))){
    #add Date
    weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                            format = '%Y-%m-%d')
  }
  
  #only take variable of interest
  x <- weather[,c('Date', variable)]
  names(x) <- c('Date', 'trials')
  #remove missing values
  x <- x[is.na(x$trials) == F,]
  
  #in case of precipitation: also remove zeros
  if(variable == 'Precip'){
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
  
  #incase there is no doy in weather, add it
  if(!'doy' %in% names(weather)){
    #in case no date in weather add it too
    if(!'Date' %in% names(weather)){
      weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                              format = '%Y-%m-%d')
    }
    weather$doy <- lubridate::yday(weather$Date)
  }
  
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

perform_gap_check <- function(weather, variable, temp_gap_threshold = 10, 
                              prec_gap_thrshold = 300){

  #set the threshold for the variable accordingly
  if(variable %in% c('Tmin', 'Tmax')){
    gap_threshold <- temp_gap_threshold
  } else if(variable == 'Precip'){
    gap_threshold <- prec_gap_thrshold
  }

  #split data per month
  var_per_month <-  split(weather, weather$Month)
  
  #perform the search for gaps on the monthly split data
  gap_flag <- map(var_per_month, function(x){
    #drop na values
    x <- x[is.na(x[,variable]) == FALSE,]
    
    #sort decreasing 
    x <- arrange(x, x[,variable])
    
    #get monthly flag
    x$gap_flag <-  get_gap_monthly(x = x[[variable]], gap_threshold = gap_threshold)
    
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
get_longterm_mean_and_sd <- function(weather, variable, doy){
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
                    mean = mean(weather[[variable]][target_days], na.rm =T), 
                    sd = sd(weather[[variable]][target_days], na.rm =T)))

}

perform_climate_outlier_check <- function(weather, variable, max_temperature_z = 6, 
                                          max_prec_threshold = 9, 
                                          max_prec_threshold_freezing = 5,
                                          prec_percentile = 0.95){
  
  if(variable %in% c('Tmin', 'Tmax')){
    
    #calculate longt term mean and sd of temperature for each day of the year for a 15 day window centered at day of interest
    clim_df <- map(unique(weather$doy), ~ get_longterm_mean_and_sd(weather = weather, variable = variable, doy = .x)) %>%
      bind_rows()
    
    #normalise temperature data
    
    weather <- merge(weather, clim_df, by = 'doy') %>%
      arrange(Date)
    
    clim_outlier <- abs((weather[,variable] - weather$mean) / weather$sd) > max_temperature_z
      
    clim_outlier <- replace_na(data = clim_outlier, replace = FALSE)
    
    return(clim_outlier)
  } else if(variable == 'Precip'){
    
    weather <- map(unique(weather$doy), ~ get_clim_percentiles_prec(weather = weather, 
                                                         doy = .x, probs = prec_percentile)) %>%
      unlist() %>%
      data.frame(doy = unique(weather$doy), percentile = .) %>%
      merge.data.frame(weather, ., by = 'doy') %>%
      arrange(Date)
    
    #in case precipitation happening at freezing temperatures, choose a lower threshold
    clim_outlier <- weather[,variable] >= ifelse((weather$Tmax + weather$Tmin) / 2 > 0, yes = weather$percentile * max_prec_threshold, 
           no = weather$percentile * max_prec_threshold_freezing)
    
    #change na to false
    clim_outlier <- replace_na(data = clim_outlier, replace = FALSE)
    
    return(clim_outlier)
  }
}


####temporal consistency checks

#iterative temperature consistency

quickker_iterat_consistency <- function(weather){
  
  #this objects determines how long the while loop goes, start value is arbetrary and just chosen, so that the while loop runs at least one time
  max_violations <- 2
  
  #object to save the flags
  tmin_flag <- tmax_flag <- tmean_flag <- rep(FALSE, nrow(weather))
  
  while(max_violations > 1){
    #check which observations are available
    tmin0 <- is.na(weather$Tmin) == FALSE
    tmin1 <- is.na(lead(weather$Tmin)) == FALSE
    tmax0 <- is.na(weather$Tmax) == FALSE
    tmax1 <- is.na(lead(weather$Tmax)) == FALSE
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

do_spike_dip_test <- function(weather, variable, dip_threshold = 25){
  flag <- abs(weather[,variable] - lead(weather[,variable])) >= dip_threshold & abs(weather[,variable] - lag(weather[,variable])) >= dip_threshold
  
  flag <- replace_na(flag[,1], FALSE)
  return(flag)
}

#lagged range test
perform_lagged_temperature_check <- function(weather, max_diff = 40){
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

###
#temperature corrobation check

perform_temperature_corrobation_check <- function(weather, weather_coords,
                                                  aux_list, aux_info,
                                                  variable,
                                                  max_station = 7, min_station = 3,
                                                  max_dist = 75, max_diff = 10){
  #get climate mean and sd for each day of target station
  #and add it to weather data frame
  weather <- map(unique(weather$doy), ~get_longterm_mean_and_sd(weather = weather, variable = variable,
                                                                doy = .x)) %>%
    bind_rows() %>%
    merge(weather, by = 'doy', all.y = TRUE) %>%
    arrange(Date)
  
  #calculate climate anomaly
  weather$anomaly <- (weather[,variable] - weather$mean) / weather$sd
  
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
    return(rep(FALSE, nrow(weather)))
  }  else if(nrow(aux_info) > max_station){
    aux_info <- aux_info[1:max_station,]
  } 
  aux_list <- aux_list[aux_info$id]
  
  aux_list <- map(aux_list, function(x){
    
    #add date and doy to aux data
    x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d")
    x$doy <- lubridate::yday(x$Date)
    #calculate climate mean and sd of aux data
    climate_df <- map(unique(x$doy), ~get_longterm_mean_and_sd(weather = x, variable = variable,
                                                               doy = .x)) %>%
      bind_rows() %>%
      merge(x, by = 'doy', all.y = TRUE) %>%
      arrange(Date)
    
    #calculate climate anomaly of aux data
    return((x[,variable] - climate_df$mean) / climate_df$sd)
  }) %>%
    map2(., aux_list, function(x,y) tibble(y, anomaly = x))
  
  #check if the absolute min distance of temperatre anomalies exceeds the threshold
  flag <- map2_lgl(weather[variable], weather$Date, function(x,y){
    get_abs_min_difference(x = x, target_date = y, variable = variable, 
                           aux_list = aux_list) >= max_diff
  })

  return(replace_na(flag, FALSE))
}

#naught check: check for wrong zeros (of either degree Fahrenheit or degree Celsius)
perform_naught_check <- function(weather){
  flag <- ifelse((weather$Tmin == -17.8 & weather$Tmax == -17.8) | (weather$Tmin == 0 & weather$Tmax == 0),
         yes = TRUE, no = FALSE)
  
  #change nas to false
  flag[is.na(flag) == TRUE] <- FALSE
  return(flag)
}

#megaconsistency check: tmin shouldnt be higher than highest tmax
#                       tmax shoudlnt be lower than lowest tmin
#                       needed because not every tmin / tmax has a "partner", so they could have slipped through the previous tests
#                       minimum of 140 observations required for the calendar month
temperature_mega_consistency_check <- function(weather, min_obs = 140){
  
  #for each month get max(tmax) and min(tmin)
  flags <- weather %>%
    group_by(Month) %>%
    mutate(flag_tmin = Tmin > max(Tmax, na.rm = T),
           flag_tmax = Tmax < min(Tmin, na.rm = T)) %>%
    ungroup() %>%
    select(Month, flag_tmin, flag_tmax) %>%
    mutate(flag_tmin = replace_na(flag_tmin, FALSE),
           flag_tmax = replace_na(flag_tmax, FALSE))
  
  #check if each month has enough observations, otherwise replace flags with FALSE
  obs_tmax <- weather %>%
    group_by(Month) %>%
    select(Tmax) %>%
    na.omit() %>%
    summarise(n = n())
  
  obs_tmin <- weather %>%
    group_by(Month) %>%
    select(Tmin) %>%
    na.omit() %>%
    summarise(n = n())
  
  too_few_obs <- which((obs_tmin$n >= min_obs) & (obs_tmax$n >= min_obs) == FALSE)
  
  #in case there are months with too few observations, replace Flag with FALSE
  if(is_empty(too_few_obs) == FALSE){
    
    flags[flags$Month == too_few_obs, c('flag_tmin', 'flag_tmax')] <- FALSE
  }
  
  return(flags[, c('flag_tmin', 'flag_tmax')])
  

}


#helper function: clear flagged values, mark which test lead to the removel
clear_flagged_data <- function(weather, variable, test_result, test_name){
  #set values to NA for positive test results
  weather[test_result, variable] <- NA
  
  #indicate which test lead to removal
  weather[test_result, paste0('flag_', variable)] <- test_name
  
  return(weather)
}

#helper function to carry out the climate percentile check for all doys
get_each_day_precipitation_percentile <- function(weather, probs = c(.3, .5, .7, .9)){
  
  #incase there is no doy in weather, add it
  if(!'doy' %in% names(weather)){
    #in case no date in weather add it too
    if(!'Date' %in% names(weather)){
      weather$Date <- as.Date(paste(weather$Year, weather$Month, weather$Day, sep = '-'),
                              format = '%Y-%m-%d')
    }
    weather$doy <- lubridate::yday(weather$Date)
  }
  
  names <- c('doy', paste0(probs * 100, '%'))
  
  map(unique(weather$doy), ~ get_clim_percentiles_prec(weather = weather, 
                                                       doy = .x,
                                                       probs = probs)) %>%
    do.call(rbind, .) %>%
    data.frame(doy = unique(weather$doy), .) %>%
    `colnames<-`(names)
}

durre_weather_quality_control <- function(weather_list, weather_info,
                                          aux_list, aux_info,
                                          region, subregion){
  
  #add a column to weather_list objects indicating which test performed positive
  #also add Date and doy
  weather_list <- map(weather_list, function(x) tibble(x, 'Date' = as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = "%Y-%m-%d"),
                                                       'Tmin_org' = x$Tmin, 
                                                       'Tmax_org' = x$Tmax, 'Precip_org' = x$Precip,
                                                       'flag_Tmin' = NA, 'flag_Tmax' = NA, 
                                                       'flag_Precip' = NA) %>%
                        mutate(doy = lubridate::yday(Date)))
  
  ####
  #basic integrity check
  ####
  cat(paste0(rep('-', 10), recycle0 = FALSE), '\n')
  cat('Basic integrity checks', '\n')
  cat('', '\n')
  
  cat('Naught check', '\n')
  #1: naught check
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, 
                                           variable = 'Tmin', 
                                           test_result = perform_naught_check(x),
                                           test_name = 'naught_check'))
  
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, 
                                                           variable = 'Tmax', 
                                                           test_result = perform_naught_check(x),
                                                           test_name = 'naught_check'))
    
  #2 duplicate check
  cat('Duplicate Check', '\n')
  #between entire years only for precipitation
  #tmin
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, variable = 'Tmin', 
                                           test_result = get_duplicated_values(weather = x, variable = 'Tmin'), 
                                           test_name = 'duplicated'))
  
  #tmax
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, variable = 'Tmax', 
                                                           test_result = get_duplicated_values(weather = x, variable = 'Tmax'), 
                                                           test_name = 'duplicated'))
  #precipitation
  weather_list <- map(weather_list, function(x) clear_flagged_data(weather = x, variable = 'Precip', 
                                                           test_result = get_duplicated_values(weather = x, variable = 'Precip'), 
                                                           test_name = 'duplicated'))
  

  #### record exceedance test
  cat('Record exceedance test', '\n')
  
  #download records
  records <- get_weather_records(region = region) %>%
    filter(Country == subregion)
  
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = test_fixed_limit(weather = x, 
                                                      variable = 'Tmin',
                                                      region = NULL,
                                                      subregion = NULL,
                                                      records = c(records$Tmin, records$Tmax)), 
                       test_name = 'record_exceedance')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = test_fixed_limit(weather = x, 
                                                      variable = 'Tmax',
                                                      region = NULL,
                                                      subregion = NULL,
                                                      records = c(records$Tmin, records$Tmax)), 
                       test_name = 'record_exceedance')
  })
  
  #Precipitation
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Precip', 
                       test_result = test_fixed_limit(weather = x, 
                                                      variable = 'Precip',
                                                      region = NULL,
                                                      subregion = NULL,
                                                      records = c(0, records$Precip)), 
                       test_name = 'record_exceedance')
  })
  
  
  #identical value streak test
  cat('Value Streak Test', '\n')
  
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = get_streaks(weather = x, 
                                                      variable = 'Tmin'), 
                       test_name = 'streaks')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = get_streaks(weather = x, 
                                                 variable = 'Tmax'), 
                       test_name = 'streaks')
  })
  
  #Precip
  #weather_list <- map(weather_list, function(x){
  #  clear_flagged_data(weather = x, variable = 'Precip', 
  #                     test_result = get_streaks(weather = x, 
  #                                               variable = 'Precip'), 
  #                     test_name = 'streaks')
  #})
  
  #calculate percentiles for each weather df, store in list
  prec_percentile_list <- map(weather_list, get_each_day_precipitation_percentile)
  
  
  cat('Frequent Identical Value Test', '\n')
  weather_list <- map2(weather_list, prec_percentile_list, function(x,y){
    clear_flagged_data(weather = x, variable = 'Precip', 
                       test_result = frequent_value_check(weather = x, 
                                                          percentile_df = y),
                       test_name = 'frequent_ident_value')
  })
  
  
  ####
  #outlier checks
  ####
  cat(rep('-', 10), '\n')
  cat('Outlier checks', '\n')
  cat('', '\n')
  
  
  #gap check
  cat('Gap check', '\n')
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = perform_gap_check(weather = x, 
                                                 variable = 'Tmin'), 
                       test_name = 'gap_check')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = perform_gap_check(weather = x, 
                                                       variable = 'Tmax'), 
                       test_name = 'gap_check')
  })
  
  #Precip
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Precip', 
                       test_result = perform_gap_check(weather = x, 
                                                       variable = 'Precip'), 
                       test_name = 'gap_check')
  })
  

  #climatological outlier
  cat('Climatological Outlier Test', '\n')
  #Tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = perform_climate_outlier_check(weather = x, 
                                                       variable = 'Tmin'), 
                       test_name = 'clim_outlier')
  })
  
  #Tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = perform_climate_outlier_check(weather = x, 
                                                                   variable = 'Tmax'), 
                       test_name = 'clim_outlier')
  })
  
  #Precip
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Precip', 
                       test_result = perform_climate_outlier_check(weather = x, 
                                                                   variable = 'Precip'), 
                       test_name = 'clim_outlier')
  })
  
  
  ######
  #Temporal consistency checks
  ######
  cat(rep('-', 10), '\n')
  cat('Temporal Consistency checks', '\n')
  cat('', '\n')
  
  #iterative temperature consistency
  cat('Iterative Consistency Check', '\n')
  
  #run the temporal temperature consistency test only once
  test_list <- map(weather_list, quickker_iterat_consistency)
  
  #apply resulst to tmin
  weather_list <- map2(weather_list, test_list, function(x,y){
    clear_flagged_data(weather = x, variable = 'Tmin', test_result = y$tmin_flag, 
                       test_name = 'iterative_consistency')
  })
  
  #same results also for tmax
  weather_list <- map2(weather_list, test_list, function(x,y){
    clear_flagged_data(weather = x, variable = 'Tmax', test_result = y$tmax_flag, 
                       test_name = 'iterative_consistency')
  })
 
  #spike - dip check
  cat('Spike/Dip Test', '\n')
  #tmin
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = do_spike_dip_test(weather = x, 
                                                                   variable = 'Tmin'), 
                       test_name = 'spike-dip')
  })
  
  #tmax
  weather_list <- map(weather_list, function(x){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = do_spike_dip_test(weather = x, 
                                                       variable = 'Tmax'), 
                       test_name = 'spike-dip')
  })

  
  #lagged temperature range check
  cat('Lagged Temperature Range Test', '\n')
  test_list <- map(weather_list, perform_lagged_temperature_check)
  
  weather_list <- map2(weather_list, test_list, function(x,y){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = y$tmin_flag, 
                       test_name = 'lagged_temperature')
  })
  
  weather_list <- map2(weather_list, test_list, function(x,y){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = y$tmax_flag, 
                       test_name = 'lagged_temperature')
  })
  
  
  #####
  #spatial consistency check
  #####
  cat(rep('-', 10), '\n')
  cat('Spatial Consistency checks', '\n')
  cat('(these usually take a little bit longer)', '\n')
  cat('', '\n')
  
  #make sure weather_info is of same order as weather_list
  weather_info <- weather_info[match(weather_info$id, names(weather_list)), ]
  
  #add cleaned weather_list to aux_list and make sure there are no duplicates
  aux_list[weather_info$id] <- weather_list
  
  #make sure that each element of aux_data is a tibble
  aux_data <- map(aux_data, tibble)
  
  #Tmin
  cat('Spatial Regression Test', '\n')
  weather_list <- imap(weather_list, function(x,id){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = test_spatial_consistency(weather = x, 
                                                              weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                 weather_info$Latitude[weather_info$id == id]),
                                                              aux_list = aux_data, 
                                                              aux_info = aux_info, 
                                                              variable = 'Tmin'), 
                       test_name = 'spatial_regression')
  })

  #Tmax
  weather_list <- imap(weather_list, function(x,id){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = test_spatial_consistency(weather = x, 
                                                              weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                 weather_info$Latitude[weather_info$id == id]),
                                                              aux_list = aux_data, 
                                                              aux_info = aux_info, 
                                                              variable = 'Tmax'), 
                       test_name = 'spatial_regression')
  })
  
  
  #temperature corrobation
  cat('Spatial Corrobation Test', '\n')
  #Tmin
  weather_list <- imap(weather_list, function(x,id){
    print(id)
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = perform_temperature_corrobation_check(weather = x, 
                                                              weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                 weather_info$Latitude[weather_info$id == id]),
                                                              aux_list = aux_data, 
                                                              aux_info = aux_info, 
                                                              variable = 'Tmin'), 
                       test_name = 'spatial_corrobation')
  })
  
  

  
  
  #Tmax
  weather_list <- imap(weather_list, function(x,id){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = perform_temperature_corrobation_check(weather = x, 
                                                                           weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                              weather_info$Latitude[weather_info$id == id]),
                                                                           aux_list = aux_data, 
                                                                           aux_info = aux_info, 
                                                                           variable = 'Tmax'), 
                       test_name = 'spatial_corrobation')
  })
  
  
  #Precipitation
  weather_list <- imap(weather_list, function(x,id){
    clear_flagged_data(weather = x, variable = 'Precip', 
                       test_result = test_precipitation_spatial_corrobation(weather = x, 
                                                                           weather_coords = c(weather_info$Longitude[weather_info$id == id], 
                                                                                              weather_info$Latitude[weather_info$id == id]),
                                                                           aux_list = aux_data, 
                                                                           aux_info = aux_info), 
                       test_name = 'spatial_corrobation')
  })
  

  ####
  #mega consistency check
  ####
  cat(rep('-', 10), '\n')
  cat('Megaconsistency checks', '\n')
  cat('', '\n')
  
  #extremes mega consistency check
  cat('Temperature Megaconsistency Test')
  test <- map(weather_list, temperature_mega_consistency_check)
  
  weather_list <- map2(weather_list, test, function(x,y){
    clear_flagged_data(weather = x, variable = 'Tmin', 
                       test_result = y$flag_tmin, 
                       test_name =  'mega_consistency')
  })
  
  weather_list <- map2(weather_list, test, function(x,y){
    clear_flagged_data(weather = x, variable = 'Tmax', 
                       test_result = y$flag_tmax, 
                       test_name =  'mega_consistency')
  })

  return(weather_list)
}
