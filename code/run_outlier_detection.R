#run outlier analysis
library(tidyverse)

#load CIMIS data
target_info <- readRDS('data/cimis-overveiw.RData')
weather_list <- readRDS('data/cimis_data.RData')

load('data/UCIPM_list_90.RData')
load('data/UCIPM_list_aux_1.RData')

#add names to the weather list
target_info$id <-  paste0('cimis_', target_info$Stat_num)
names(weather_list) <- target_info$id

#combine all auxiliar data to one
aux_data <- c(aux_data, weather_list, UCIPM_list_90)
rm(UCIPM_list_90)

#load weather station info data
load('data/UCIPM_overview_all.RData')
UCIPM_WS_51s_10s <-  rename(UCIPM_WS_51s_10s, id = chillR_code)

#merge info of auxiliary weather stations
aux_overview <- merge.data.frame(UCIPM_WS_51s_10s, target_info, 
                                 by = c('id', 'Name', 'Latitude', 'Longitude'), all = T) %>%
  dplyr::select(id, Name, Longitude, Latitude)

rm(UCIPM_WS_51s_10s)


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

