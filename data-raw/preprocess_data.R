library(chillR) #function to load temperature data

# 
# target_weather <- load_temperature_scenarios(path = "../applied_weatherQC_CA/prepared_weather_data/",
#                                              prefix = "target-station")
# 
# aux_weather <- load_temperature_scenarios(path = "../applied_weatherQC_CA/prepared_weather_data/",
#                                           prefix = "aux-station")
# 
# cimis_info <- read.csv("../applied_weatherQC_CA/cimis_info.csv")
# 
# target_info <- cimis_info[1,c("id", "Name", "Longitude", "Latitude")]
# 
# aux_info <- read.csv('../applied_weatherQC_CA/quality_control_aux-info.csv')
# 
# #subset for the weather stations close to the target_station
# 
# aux_info$dist <-  round(sp::spDistsN1(pts = as.matrix(aux_info[, c("Longitude", "Latitude")]),
#                                       pt = c(target_info$Longitude, target_info$Latitude),
#                                       longlat = TRUE), 2)
# 
# aux_info <- aux_info %>%
#   filter(dist > 0 & dist <= 75) %>%
#   select(-dist)
# 
# aux_weather <- aux_weather[aux_info$id]
# 
# save_temperature_scenarios(generated_temperatures = aux_weather,
#                            path = 'data-raw/',
#                            prefix = 'neighbour-weather')
# 
# write.csv(aux_info, file = 'data-raw/neighbour_info.csv', row.names = F)
# 
# 
# 
# write.csv(target_info, file = 'data-raw/weather_info.csv', row.names = F)
# 
# #write target weather station
# write.csv(target_weather[[1]], file = 'data-raw/target-weather_cimis_2.csv',
#           row.names = F)


#load meta_data of target and neighbour stations
target_info <- read.csv('data-raw/weather_info.csv')
neighbour_info <- read.csv('data-raw/neighbour_info.csv')

#load weather observation of target and neighbour stations
target_weather <- read.csv('data-raw/target-weather_cimis_2.csv')
neighbour_weather <- load_temperature_scenarios(path = 'data-raw/',
                                             prefix = 'neighbour-weather')

#add date and doy to weather data
target_weather$Date <- as.Date(paste(target_weather$Year, target_weather$Month,
                                     target_weather$Day, sep = '-'), format = "%Y-%m-%d")
target_weather$doy <- lubridate::yday(target_weather$Date)

neighbour_weather <- purrr::map(neighbour_weather, function(x){
  x$Date <- as.Date(paste(x$Year, x$Month,
                                       x$Day, sep = '-'), format = "%Y-%m-%d")
  x$doy <- lubridate::yday(x$Date)
  return(x)
})

#download cimis stations and ucipm stations
#save as objects in the end

usethis::use_data(target_info, overwrite = TRUE)
usethis::use_data(neighbour_info, overwrite = TRUE)
usethis::use_data(target_weather, overwrite = TRUE)
usethis::use_data(neighbour_weather, overwrite = TRUE)
