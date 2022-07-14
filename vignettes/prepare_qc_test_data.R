#prepare test results for vignette
library(weatherQC)

weather_list <- list(target_weather)
names(weather_list) <- target_info$id

result_costa <- weather_qc_costa(weather_list = weather_list, 
                 weather_info = target_info,
                 aux_list = neighbour_weather,
                 aux_info = neighbour_info,
                 region = "USA", 
                 subregion = "California")

start <- Sys.time()
result_durre <- weather_qc_durre(weather_list = weather_list, 
                                 weather_info = target_info,
                                 aux_list = neighbour_weather,
                                 aux_info = neighbour_info,
                                 mute = F, 
                                 region = "USA", 
                                 subregion = "California")
end <- Sys.time()
end - start

 
write.csv(result_costa[[1]],file = 'vignettes/qc_costa_example.csv',row.names = F)
write.csv(result_durre[[1]],file = 'vignettes/qc_durre_example.csv',row.names = F)
