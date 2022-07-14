## code to prepare `neighbour_weather` dataset goes here

neighbour_weather <-  chillR::load_temperature_scenarios(path = 'data-raw/',
                                                prefix = 'neighbour-weather')

#add date and doy
neighbour_weather <- purrr::map(neighbour_weather, function(x){
  x$Date <- as.Date(paste(x$Year, x$Month,
                          x$Day, sep = '-'), format = "%Y-%m-%d")
  x$doy <- lubridate::yday(x$Date)
  return(x)
})

usethis::use_data(neighbour_weather, overwrite = TRUE)
