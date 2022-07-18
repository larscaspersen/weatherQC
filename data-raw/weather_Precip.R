## code to prepare `weather_Precip` dataset goes here

weather_list <- chillR::load_temperature_scenarios(path = 'data-raw/',
                                                   prefix = 'cleaned')

#take data.frames, drop everything except Day, Year, Month, Tmin; then merge
weather_Precip <- purrr::map(weather_list, function(x){
  x[,c('Year', 'Month', 'Day', 'Precip')] %>%
    filter(Year %in% c(1990, 1991)) %>%
    select(Precip) %>%
    pull()
}) %>%
  cbind.data.frame()

date_df <- chillR::make_all_day_table(data.frame(Year = c(1990, 1991), Month = c(1,12), 
                                                 Day = c(1,31), Tmin = NA, Tmax = NA),
                                      no_variable_check = T)

weather_Precip <- tibble(date_df[,c('Year', 'Month', 'Day')], weather_Precip)

#add date
weather_Precip <- weather_Precip %>%
  dplyr::mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")) %>%
  dplyr::relocate(Date)

usethis::use_data(weather_Precip, overwrite = TRUE)
