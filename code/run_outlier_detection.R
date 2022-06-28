#run outlier analysis
library(tidyverse)

# #load CIMIS data
# target_info <- readRDS('data/cimis-overveiw.RData')
# weather_list <- readRDS('data/cimis_data.RData')
# 
# load('data/UCIPM_list_90.RData')
# load('data/UCIPM_list_aux_1.RData')
# 
# #add names to the weather list
# target_info$id <-  paste0('cimis_', target_info$Stat_num)
# names(weather_list) <- target_info$id
# 
# #combine all auxiliar data to one
# aux_data <- c(aux_data, weather_list, UCIPM_list_90)
# rm(UCIPM_list_90)
# 
# #load weather station info data
# load('data/UCIPM_overview_all.RData')
# UCIPM_WS_51s_10s <-  rename(UCIPM_WS_51s_10s, id = chillR_code)
# 
# #merge info of auxiliary weather stations
# aux_overview <- merge.data.frame(UCIPM_WS_51s_10s, target_info, 
#                                  by = c('id', 'Name', 'Latitude', 'Longitude'), all = T) %>%
#   dplyr::select(id, Name, Longitude, Latitude)
# 
# rm(UCIPM_WS_51s_10s)
# 
# saveRDS(aux_data, file = 'data/quality_control_aux-data.RData')
# saveRDS(aux_overview, file = 'data/quality_control_aux-info.RData')
# saveRDS(target_info, file = 'data/quality_control_target-info.RData')
# saveRDS(weather_list, file = 'data/quality_control_target-data.RData')


#load target and aux data
aux_data <- readRDS('data/quality_control_aux-data.RData')
aux_info <- readRDS('data/quality_control_aux-info.RData')
weather_list <- readRDS('data/quality_control_target-data.RData')
weather_info <- readRDS('data/quality_control_target-info.RData')

source('code/outlier-functions.R')


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
durre_weather_quality_control(weather_list = weather_list,
                              weather_info =  weather_info,
                              aux_list = aux_data, 
                              aux_info = aux_info,
                              region = 'USA',
                              subregion = 'California')
end_time <- Sys.time()














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

