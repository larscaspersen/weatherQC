library(chillR) #function to load temperature data

#load meta_data of target and neighbour stations
target_info <- read.csv('data-raw/weather_info.csv')
neighbour_info <- read.csv('data-raw/neighbour_info.csv')

#load weather observation of target and neighbour stations
target_weather <- read.csv('data-raw/target_weather_cimis_7.csv')
neighbour_weather <- load_temperature_scenarios(path = 'data-raw/',
                                             prefix = 'neighbour_weather')

#download cimis stations and ucipm stations
#save as objects in the end

usethis::use_data(target_info, overwrite = TRUE)
usethis::use_data(neighbour_info, overwrite = TRUE)
usethis::use_data(target_weather, overwrite = TRUE)
usethis::use_data(neighbour_weather, overwrite = TRUE)
