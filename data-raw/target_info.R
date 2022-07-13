## code to prepare `target_info` dataset goes here

#load meta_data of target and neighbour stations
target_info <- read.csv('data-raw/weather_info.csv')

usethis::use_data(target_info, overwrite = TRUE)
