## code to prepare `target_weather` dataset goes here

#load weather observation of target and neighbour stations
target_weather <- read.csv('data-raw/target-weather_cimis_2.csv')

#add date and doy to weather data
target_weather$Date <- as.Date(paste(target_weather$Year, target_weather$Month,
                                     target_weather$Day, sep = '-'), format = "%Y-%m-%d")
target_weather$doy <- lubridate::yday(target_weather$Date)

usethis::use_data(target_weather, overwrite = TRUE)
