#run outlier analysis
library(tidyverse)
library(chillR)

#load target and aux data
aux_data <- chillR::load_temperature_scenarios(path = 'data/prepared_weather_data/',
                                               prefix = 'aux-station')
aux_info <- read.csv('data/quality_control_aux-info.csv')
weather_list <- chillR::load_temperature_scenarios(path = 'data/prepared_weather_data/',
                                                   prefix = 'target-station')
weather_info <- read.csv('data/cimis_info.csv')

source('code/outlier-functions.R')


library(chillR)

fixed_limit_test(weather = KA_weather, region = 'world', subregion = 'Germany', 
                 variable = 'Tmin')

chillR::KA_weather


#run outlier detection costa 2021 on first element of weather_list for Tmin
#define variables
id <- target_info$id[1]
weather <- weather_list[[id]]
target_coord <- c(target_info$Longitude[target_info$id == id],
                  target_info$Latitude[target_info$id == id])

#run costa 2021 quality control
test_result <- weather_qc_costa(weather = weather, weather_coords = target_coord, variable = 'Tmin',aux_list = aux_data, aux_info = aux_info, level = 'USA', country = 'California')

write.csv(test_result, file = 'data/qc_costa_tmin_example.csv', row.names = FALSE)



###run durre qc

start_time <- Sys.time()
weather_result <- durre_weather_quality_control(weather_list = weather_list,
                              weather_info =  weather_info,
                              aux_list = aux_data, 
                              aux_info = aux_info,
                              region = 'USA',
                              subregion = 'California')
end_time <- Sys.time()

saveRDS(weather_result, file = 'data/quality-control-durre_results-CIMIS.RData')

#took roughly 4 hours

devtools::load_all()
library(weatherQC)
start_time <- Sys.time()
weather_result <- weather_qc_durre(weather_list = weather_list,
                                   weather_info =  weather_info,
                                   skip_spatial_test = T,
                                   region = 'USA',
                                   subregion = 'California')
end_time <- Sys.time()
end_time - start_time
#without spatial coherence test takes about 2 minutes















id <- target_info$id[1]
weather_coords <- c(target_info$Longitude[1], target_info$Latitude[1])

test_res <- test_for_outlier(weather = weather_list[[id]], weather_coords = weather_coords, 
                                 var = 'Tmin', aux_list = aux_data, aux_info = aux_overview, 
                                 level = 'world', country = 'United States')

test <- precipitation_spatial_corrobation_test(weather = weather, weather_coords = weather_coords,
                                       aux_info = aux_overview, aux_list = aux_data)

#combine test results with weather data
test_res <- tibble(weather, test_res)

sum(test_res$outlier)

test_res %>%
  group_by(QC_Tmin) %>%
  summarise(n = n())

sub <- test_res %>%
  filter(QC_Tmin %in% c('R', 'S') & is.na(Tmin) == F)

sum(sub$outlier) / nrow(sub)



#visualize the 'identified' outlier
sum(test_res$outlier) / nrow(test_res)

sub_1 <- test_res %>%
  filter(outlier == T & !QC_Tmin %in% c('R', 'S') )

