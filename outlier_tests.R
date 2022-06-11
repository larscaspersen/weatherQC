#scrap temperature records from wikipedia
#follow example of https://towardsdatascience.com/scraping-data-from-wikipedia-tables-3efa04c6b53f
library(tidyverse)
library(rvest)

#function to get temperature records
get_temp_records <- function(region = 'world'){
  
  if(region == 'world'){
    
    url <- 'https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_extreme_temperatures'
    
    temperature_html <- read_html(url)
    
    temperature_html %>% 
      html_nodes(css = "table")
    
    temp_table <- temperature_html %>% 
      html_nodes(css = "table") %>% 
      nth(1) %>% 
      html_table(fill = TRUE, convert = F)
    
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



#load cimis data
load('data/CIMIS_list_90.RData')
weather <- CIMIS_list_90[[1]]

load('data/CIMIS_overview_90.RData')
lat <- CIMIS_WS_90$Latitude[1]
lon <- CIMIS_WS_90$Longitude[1]

#fixed limit test
#--> use the min and max record of california

cal_rec <- records %>%
  filter(State == 'California')

var <- 'Tmin'

tmin_flags <- tibble(fixed_limit_test = rep(FALSE, nrow(weather)),
       variable_limit_test = rep(FALSE, nrow(weather)),
       temporal_continuity_test = rep(FALSE, nrow(weather)),
       spatial_consistency_test = rep(FALSE, nrow(weather)),
       var_consistency_test = rep(FALSE, nrow(weather)))

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
    if(!level %in% valid_countries){
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
  
  return(ifelse(is.na(weather[,var]),yes = FALSE, no = (weather[,var] < records[1]) | (weather[,var] > records[2])))
  
}

#only works with internet connection
tmin_flags[, 'fixed_limit_test'] <- fixed_limit_test(weather = weather, 
                                                     level = 'USA', 
                                                     country = 'California', 
                                                     var = 'Tmin')

###variable limit test

variable_limit_test

#take climate normals of world clim
# library(raster)
# library(sp)
# 
# r <- getData("worldclim",var="tmin",res=0.5, lon = lon, lat = lat)
# 
# meta_data <- CIMIS_WS_90[1,]
# #create spatial dataframe
# coordinates(meta_data) <- ~Longitude+Latitude
# 
# #extract climate normals of worldclim data
# values <- extract(r,meta_data) 
# values <- values / 10
# 
# #remove downloaded geospatial data
# rm(r)

#function to add date column 
add_date <- function(x){
  x$Date <- as.Date(paste(x$Year, x$Month, x$Day, sep = '-'), format = '%Y-%m-%d')
  return(x)
}

#add date
weather <- add_date(weather)

#split to monthly groups
monthly_weather <- split(weather, f = weather$Month)

#get monthly percentiles
monthly_perc <- purrr::map(monthly_weather, ~quantile(.x[,var], probs = c(0.01, 0.99), na.rm = T))

var_consistency <-  purrr::map2(monthly_weather, monthly_perc, function(x,y)ifelse(is.na(x[,var]),yes = F, no = ((x[,var]<y[1])|x[,var]>y[2])))

#bring back the var consistency to original format
var_test <- purrr::map2(var_consistency, monthly_weather, cbind) %>%
  do.call(rbind, .) %>%
  arrange(Date)

#also test for yearly quantiles
yearly_quan <- quantile(weather[,var], probs = c(0.01, 0.99), na.rm = T)

yearly_var_test <- ifelse(is.na(weather[,var]), yes = F, no = ((weather[, var] < yearly_quan[1])|(weather[,var]>yearly_quan[2])))

tmin_flags[, 'variable_limit_test'] <- (yearly_var_test | var_test[,1])


###temporal continuity
#calculate absolute difference to next day

temporal_continuity_test <-  function(weather, var, prob = 0.995){
  #calculate difference to next day, append one NA to keep same length
  diffs <- c(abs(diff(as.matrix(weather[,var]))), NA)
  
  #get the quantile of the jumps, everything above the quantile is labelled as suspicious
  jump_quan <- quantile(diffs, probs = prob, na.rm = T)
  
  return(ifelse(is.na(diffs), yes = F, no = (diffs > jump_quan)))
  
}

tmin_flags[, 'temporal_continuity_test'] <- temporal_continuity_test(weather = weather,
                                                                     var = var)


#### consitstency between variables (I ignore the mean temperature for now)
temperature_consistency_test <- function(weather, probs = 0.99){
  cons1 <-  weather$Tmin > weather$Tmax
  
  #min temperature should be lower than tmean
  cons2 <- weather$Tmin > weather$Tmean
  
  #tmean should not be too different from (tmin + tmax)/2
  diff_tmean <- abs(((weather$Tmin + weather$Tmax)/2) - weather$Tmean)
  perc_diff_tmean <- quantile(diff_tmean, prob = probs, na.rm = T)
  cons3 <- diff_tmean > perc_diff_tmean
  
  return(ifelse(is.na(weather[,var]), yes = F, no = (cons1 | cons2 | cons3)))
  
}

#if any of cons1, cons2 or cons3 is true, then tmin and tmax is labelled as doubious
tmin_flags[, 'var_consistency_test'] <- temperature_consistency_test(weather = weather)



#compute distance to weather station
#extract data 
#drop stations not fulfilling mininmum overlap of 40 days
#calculate correlation coefficient with x, drop stations with value lower than 0.8
#create selection of days for each day that is closest to the target observation from a three day window
#perform regression
#
#
load('data/CIMIS_list_90.RData')
load('data/UCIPM_list_90.RData')
load('data/UCIPM_list_aux_1.RData')

CIMIS_WS_90$id <-  paste0('cimis_', CIMIS_WS_90$Stat_num)
names(CIMIS_list_90) <- CIMIS_WS_90$id

aux_data <- c(aux_data, CIMIS_list_90, UCIPM_list_90)
rm(CIMIS_list_90, UCIPM_list_90)

load('data/UCIPM_overview_all.RData')
UCIPM_WS_51s_10s <-  rename(UCIPM_WS_51s_10s, id = chillR_code)

aux_overview <- merge.data.frame(UCIPM_WS_51s_10s, CIMIS_WS_90, 
                 by = c('id', 'Name', 'Latitude', 'Longitude'), all = T) %>%
                dplyr::select(id, Name, Longitude, Latitude)

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
  
  ####temp
  aux_list <- aux_list[aux_info$id]
  ####temp
  

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

spat_flag <- spatial_consistency_test(weather = weather, 
                         weather_coords = c(CIMIS_WS_90$Longitude[1], CIMIS_WS_90$Latitude[1]), 
                         aux_list = aux_data, aux_info = aux_overview, var = 'Tmin')






 
 library(tidyverse)
 
 weather <- gw$weather %>%  
   dplyr::select(Date, Year, Month, Day,`Minimum Air Temperature`,
                 `Average Air Temperature`, `Maximum Air Temperature`,Precipitation) %>%
   dplyr::rename(Tmin = `Minimum Air Temperature`,
                 Tmean = `Average Air Temperature`,
                 Tmax = `Maximum Air Temperature`,
                 Prec = Precipitation) 
 
 
 
 
 
 
stat_list <- data.frame("Station Number" = c("119", "139", "6"),
                        Latitude = c(38.49500, 38.50126, 38.53569),
                        Longitude = c(-122.0040, -121.9785, -121.7764),
                        Start_date =c("1993-08-21 UTC", "1998-06-15 UTC", "1982-07-17 UTC"),
                        End_date = c("1995-01-25", "2016-03-06", "2016-03-06"))






meta_data

gw <- handle_cimis(action = "download_weather",location = "2",
                   time_interval = c(1990 ,2020))

#remove values which are flagged to be suspicious
drop <- gw$weather$`QC for Maximum Air Temperature` %in% c('R', 'S', 'I')
gw$weather[drop, ]$`Maximum Air Temperature`

drop <- gw$weather$`QC for Minimum Air Temperature` %in% c('R', 'S', 'I')
gw$weather[drop, ]$`Minimum Air Temperature`

drop <- gw$weather$`QC for Precipitation` %in% c('I', 'S', 'H', 'R')
gw$weather[drop, ]$Precipitation
gw$weather[drop, ]$`QC for Precipitation`







#evaluate the dataset with the plausability checks and then see which data is marked

#check quality control flags from other sources
#ucipm, gsod, dwd
load('data/UCIPM_overview_90.RData')
dat <- handle_ucipm(action = "download_weather", location = UCIPM_WS_90$chillR_code[1], time_interval = c(1951,2021))

#ucipm did not state on the quality control and did not supply qc flags

load('data/GSOD_overview_90.RData')

#use gsodr package
inventory <- GSODR::get_inventory()
inventory %>%
  filter(CTRY == 'US', STATE == 'CA', BEGIN <= 19510101, END >= 20201231)
tbar <- GSODR::get_GSOD(station = '722860-23119', years = 1951:2020)

str(tbar)
vignette("GSODR", package = "GSODR")

as.factor(tbar$PRCP_ATTRIBUTES)
rm(tbar, dat)

#handle_gsod(action = "download_weather", location = GSOD_WS_90$STNID[1], time_interval = c(1951,2021))
#

