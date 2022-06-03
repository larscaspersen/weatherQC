########
#test evaluation on spei
#########
library(tidyverse)
library(chillR)

#load functions
source('code/patching_function.R')


#prepare input data, only needed for first time, because then i saved the formated files

# #read weather data
# load('data/UCIPM_list_90.RData')
# load('data/UCIPM_list_aux_1.RData')
# load('data/UCIPM_overview_all.RData')
# 
# #combine everything to one list
# weather_list <- c(UCIPM_list_90, aux_data)
# rm(aux_data, UCIPM_list_90)
# 
# #####
# #re-organize the data per variable
# #####
# 
# #take column of list element, paste it to new list and then cbind everything
# tmin <- lapply(weather_list, function(x){
#   x$Tmin
# })
# tmin <- do.call(cbind.data.frame, tmin)
# 
# tmax <- lapply(weather_list, function(x){
#   x$Tmax
# })
# tmax <- do.call(cbind.data.frame, tmax)
# 
# prcp <- lapply(weather_list, function(x){
#   x$Precip
# })
# prcp <- do.call(cbind.data.frame, prcp)
# 
# #add columns of Date, Year, Month, Day to the dataframes
# ymd <- weather_list[[1]][,c('Year', 'Month', 'Day')]
# 
# #add column of date
# ymd$Date <- as.Date(paste0(ymd$Year,'-', ymd$Month, '-', ymd$Day), format = '%Y-%m-%d')
# 
# #add year, month, day and date to existing variables
# prcp <- cbind(ymd, prcp)
# tmin <- cbind(ymd, tmin)
# tmax <- cbind(ymd, tmax)
# 
# #save prepared data
# write.csv(tmin, file = 'data/CA-tmin.csv', row.names = F)
# write.csv(tmax, file = 'data/CA-tmax.csv', row.names = F)
# write.csv(prcp, file = 'data/CA-prcp.csv', row.names = F)
# 
# 
# #adjust meta data
# meta_data <- UCIPM_WS_51s_10s[,c('chillR_code', 'Name', 'Longitude', 'Latitude')]
# colnames(meta_data) <- c('id', 'name', 'longitude', 'latitude')
# 
# #download elevation data
# ll_prj <- "EPSG:4326"
# mts_sp <- sp::SpatialPoints(sp::coordinates(meta_data[,c('longitude', 'latitude')]), 
#                             proj4string = sp::CRS(SRS_string = ll_prj))
# elevation <- elevatr::get_elev_point(locations = mts_sp)
# 
# #add to meta-data
# meta_data$elevation <- elevation$elevation
# 
# #remove data which is no longer needed
# rm(elevation, ll_prj, mts_sp, ymd, UCIPM_WS_51s_10s, weather_list)
# 
# write.csv(meta_data, file = 'data/CA-metadata.csv', row.names = F)




#read meta-data
meta_data <- read.csv('data/CA-metadata.csv')

#read weather data
prcp <- read.csv('data/CA-prcp.csv')
tmin <- read.csv('data/CA-tmin.csv')
tmax <- read.csv('data/CA-tmax.csv')

#make sure that order in weather data is same as in metadata
prcp <- prcp[,c('Year', 'Month', 'Day', 'Date', meta_data$id)]
tmin <- tmin[,c('Year', 'Month', 'Day', 'Date', meta_data$id)]
tmax <- tmax[,c('Year', 'Month', 'Day', 'Date', meta_data$id)]

#prepare precipitation data, so that everything below set threshold is treated as 0
prcp_threshold <- 1

#set precipitation data below threshold to zero
for(station in meta_data$id){
  prcp[(prcp[,station] < prcp_threshold) & !is.na(prcp[,station]) ,station] <- 0
}


#get a list of periods
period_list <- lapply(meta_data$id, function(x){
  
  #get longest period of tmin, tmax and prcp combined
  longest_coverage <- na.contiguous(ts(data.frame(tmin = tmin[,x], tmax = tmax[,x], prcp = prcp[,x])))
  return(tsp(longest_coverage)[1:2])
})

#check that for every evalaution period, there is no na 
test <- lapply(1:nrow(meta_data), function(x){
  sum(is.na(prcp[period_list[[x]][1]:period_list[[x]][2], meta_data$id[x]]))+
    sum(is.na(tmin[period_list[[x]][1]:period_list[[x]][2], meta_data$id[x]]))+
    sum(is.na(tmax[period_list[[x]][1]:period_list[[x]][2], meta_data$id[x]]))
} )
sum(unlist(test))
#--> no evaluation period contains na for any variable




# #get info abput the evaluation period length, median, min, max
# period_length <- lapply(period_list, function(x){
#   x[2] - x[1] + 1
# })
# rm(period_length)

# period_length <- do.call(c, period_length)
# median(period_length) / 365
# #8.2 years
# min(period_length) / 365
# #1.1 years
# max(period_length) / 365
# #45.4 years

# #get info on missingness
# target <- meta_data$id
# 
# #get info on missingness of each variable
# prcp_missing <- apply(prcp[, target], MARGIN = 2, function(x){
#   (sum(is.na(x)) / length(x)) * 100
# })
# median(prcp_missing)
# #2.1%
# min(prcp_missing)
# #0%
# max(prcp_missing)
# #35.4%

# tmin_missing <- apply(tmin[, target], MARGIN = 2, function(x){
#   (sum(is.na(x)) / length(x)) * 100
# })
# median(tmin_missing)
# #4.4%
# min(tmin_missing)
# #0.2%
# max(tmin_missing)
# #36.5%

# tmax_missing <- apply(tmax[, target], MARGIN = 2, function(x){
#   (sum(is.na(x)) / length(x)) * 100
# })
# median(tmax_missing)
# #4.4%
# min(tmax_missing)
# #0.2%
# max(tmax_missing)
# #36.5%
#rm(tmax_missing, tmin_missing, prcp_missing)

# #visualise evaluation period
# 
# library(ggplot2)
# 
# p0 <- naniar::vis_miss(data.frame(Tmin = tmin$STOCKTON.C, Tmax = tmax$STOCKTON.C,
#                                   Prcp = prcp$STOCKTON.C),warn_large_data = F,sort_miss = T)
# 
# p1 <- naniar::vis_miss(data.frame(Tmin = tmin$STOCKTON.C, Tmax = tmax$STOCKTON.C,
#                             Prcp = prcp$STOCKTON.C),warn_large_data = F,sort_miss = T) +
#   geom_linerange(aes(ymin = period_list[[1]][1], ymax = period_list[[1]][2]), color = "red", size = 1)
# 
# p2 <- naniar::vis_miss(data.frame(Tmin = tmin$STOCKTON.C, Tmax = tmax$STOCKTON.C,
#                                   Prcp = prcp$STOCKTON.C),warn_large_data = F,sort_miss = T) +
#   geom_linerange(aes(ymin = period_list[[1]][1], ymax = period_list[[1]][2]), color = "red", size = 1) +
#   coord_cartesian(ylim = c(8200, 7700))
# 
# 
# p3 <- naniar::vis_miss(data.frame(Tmin = tmin$STOCKTON.C, Tmax = tmax$STOCKTON.C,
#                                   Prcp = prcp$STOCKTON.C),warn_large_data = F,sort_miss = T) +
#   geom_linerange(aes(ymin = period_list[[1]][1], ymax = period_list[[1]][2]), color = "red", size = 1) +
#   coord_cartesian(ylim = c(9500, 9000))
# 
# ggsave(p0, filename = 'figures/example_CA_eval_period_blank.jpeg')
# ggsave(p1, filename = 'figures/example_CA_eval_period.jpeg')
# ggsave(p2, filename = 'figures/example_CA_eval_period_zoom1.jpeg')
# ggsave(p3, filename = 'figures/example_CA_eval_period_zoom2.jpeg')

#test patch_mean on all weather data
#set which patch methods to use

#set which patch methods to use
patch_methods <- c("patch_mean", "patch_normal_ratio", 
                   'patch_mice', 'patch_amelia', 'patch_climatol',
                   'patch_pca', 'patch_pca', 'patch_idw')


#indicate if function either patches the whole data frame or if it patches one column after another
method_patches_everything = c(F, F, T, T, T, T, T, T)


#define additional arguments needed for the function call, needs to be of same length as patch methods
additional_args_temp <- list(list(n_donors = 5),
                        list(n_donors = 5),
                        list(rain_data = F, n.impute = 5, max.iter = 5, parallel = T),
                        list(rain_data = F, n.impute = 5, parallel = 'snow'),
                        NA,
                        list(method = 'nipals', nPcs = 3),
                        list(method = 'ppca', nPcs = 3),
                        list(phi = 2))

additional_args_rain <- list(list(n_donors = 5),
                             list(n_donors = 5),
                             list(rain_data = T, prcp_threshold = prcp_threshold, n.impute = 5, max.iter = 5, parallel = T),
                             list(rain_data = T, prcp_threshold = prcp_threshold, n.impute = 5, parallel = 'snow'),
                             NA,
                             list(method = 'nipals', nPcs = 3),
                             list(method = 'ppca', nPcs = 3),
                             list(phi = 2))

#at first determine the longest period per station in which tmin, tmax and prcp have data together
weather_list <- list(list(tmin, additional_args_temp), list(tmax, additional_args_temp), list(prcp, additional_args_rain))
weather_vars <- c('tmin', 'tmax', 'prcp')

#missingness to evaluate
p_missing <- 0.4



#carryout patching evaluation, return the whole evaluation period, as the spei index is calculated for it
eval_list <- lapply(weather_list, function(x){
  get_eval_one_station(weather = x[[1]], target = meta_data$id, meta_data = meta_data,
                       patch_methods = patch_methods, p_missing = p_missing, 
                       additional_args = x[[2]], method_patches_everything = method_patches_everything,
                       period = period_list, return_data = 'evaluation_period')
})

#save workspace
#save.image(file = "patch_california_workspace.RData")


#stopped here, restart from this point tomorrow
load('patch_california_workspace.RData')


#calculation done, do plotting

# for spei calculation i need to bring the data in differetn format:
# currently data is organised as: one df per variable, one column for each patch_method
# goal: have a list, for each station one df, each df contain tmin, tmax, prec of one station, long format indicates original, patch method etc

#indicate which df is for which variable
names(eval_list) <- weather_vars

#add variable name to df
for(var in weather_vars){
  eval_list[[var]]['variable'] <- var
}

#bind everything by row
eval <- do.call(rbind, eval_list)
write.csv(eval, file = 'data/CA-patch_eval.csv', row.names = F)

eval <- read.csv('data/CA-patch_eval.csv')

#####
#plot of daily imputation
#####

#subset only imputed data
eval_daily <- eval[eval$new_na,]

#bring to long format
eval_daily_long <- reshape2::melt(eval_daily, 
                                  id.vars = c('Date', 'Year', 'Month', 'Day', 'station','original', 'new_na', 'variable'),
                                  variable.name = 'patch_method')

eval_daily_long$value[eval_daily_long$value < prcp_threshold & eval_daily_long$variable == 'prcp'] <- 0
eval_daily_long$original[eval_daily_long$original < prcp_threshold & eval_daily_long$variable == 'prcp'] <- 0

sum(is.na(eval_daily_long$value))
sum(is.na(eval_daily_long$original))

#which method had nas?
eval_daily_long$station <- as.factor(eval_daily_long$station)
summary(eval_daily_long[is.na(eval_daily_long$value),])
#--> remove method 
eval_daily_long <- dplyr::filter(eval_daily_long, patch_method != 'patch_mean')

#--> there are few cases where patch_mean failed, remove them from evaluation
eval_daily_long <- eval_daily_long[!is.na(eval_daily_long$value),]

#check cases where patch_normal_ratio created weird results
eval_daily_long <- eval_daily_long[which(!(abs(eval_daily_long$value) > 50 & eval_daily_long$variable == 'tmin')),]


## qqplot with r2, rpiq and rmse value

#calculate metrics
r2 <- eval_daily_long %>%
  group_by(patch_method, variable) %>%
  summarise(r2 = cor(value, original)^2,
            rmse = chillR::RMSEP(predicted = value, observed = original))
#set theme
theme_set(theme_bw(14))


#qqplot of precipitation
p4 <- eval_daily_long %>%
  filter(variable == 'prcp') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, col = 'red', linetype = 'dashed') + 
  geom_label(data = r2[r2$variable == 'prcp',], 
             aes(x = Inf, y = Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2),'\nRMSE = ', round(rmse, digits = 2),  sep = " ")),
             hjust = 1, vjust = 1)+
  xlab('Predicted Daily Precipitation [mm]')+
  ylab('Observed Daily Precipitation [mm]')+
  facet_wrap(~patch_method)
ggsave(plot = p4, filename = 'figures/eval_spei/CA_qq_daily_prcp.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')

#qqplot of tmin
p5 <- eval_daily_long %>%
  filter(variable == 'tmin') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, col = 'red', linetype = 'dashed') + 
  geom_label(data = r2[r2$variable == 'tmin',], 
             aes(x = Inf, y = Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2),'\nRMSE = ', round(rmse, digits = 2),  sep = " ")),
             hjust = 1, vjust = 1)+
  xlab('Predicted Daily Minimum Temperature [°C]')+
  ylab('Observed Daily Minimum Temperature [°C]')+
  facet_wrap(~patch_method)
ggsave(plot = p5, filename = 'figures/eval_spei/CA_qq_daily_tmin.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')


#qqplot of tmax
p6 <- eval_daily_long %>%
  filter(variable == 'tmax') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, col = 'red', linetype = 'dashed') + 
  geom_label(data = r2[r2$variable == 'tmax',], 
             aes(x = Inf, y = Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2),'\nRMSE = ', round(rmse, digits = 2),  sep = " ")),
             hjust = 1, vjust = 1)+
  xlab('Predicted Daily Maximum Temperature [°C]')+
  ylab('Observed Daily Maximum Temperature [°C]')+
  facet_wrap(~patch_method)
ggsave(plot = p6, filename = 'figures/eval_spei/CA_qq_daily_tmax.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')






### cumulative density plot

#for density plot I also need original data in long format
eval_long2 <- reshape2::melt(eval_daily, 
                             id.vars = c('Date', 'Year', 'Month', 'Day', 'station', 'new_na', 'variable'),
                             variable.name = 'patch_method')

eval_long2$value[eval_long2$value < prcp_threshold & eval_long2$variable == 'prcp'] <- 0

#colour palette with original as black
cbp1 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1_without_org <- c("#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#check the differences in density plot
p7 <- eval_long2 %>%
  filter(variable == 'prcp', patch_method != 'patch_mean') %>%
  ggplot(aes(value+1, col = patch_method)) + stat_ecdf(geom = "step") +
  coord_trans(x="log2") +
  xlab('Log(Daily Precipitation [mm] + 1)') + 
  ylab('Cumulative Density')+
  scale_colour_manual(values=cbp1)
ggsave(plot = p7, filename = 'figures/eval_spei/CA_density_daily_prcp.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')


p8 <- eval_long2 %>%
  filter(variable == 'tmin', patch_method != 'patch_mean') %>%
  ggplot(aes(value+1, col = patch_method)) + stat_ecdf(geom = "step") +
  xlab('Daily Minimum Temperature [°C]') + 
  ylab('Cumulative Density')+
  scale_colour_manual(values=cbp1)
ggsave(plot = p8, filename = 'figures/eval_spei/CA_density_daily_tmin.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')


p9 <- eval_long2 %>%
  filter(variable == 'tmax', patch_method != 'patch_mean') %>%
  ggplot(aes(value+1, col = patch_method)) + stat_ecdf(geom = "step") +
  xlab('Daily Maximum Temperature [°C])') + 
  ylab('Cumulative Density')+
  scale_colour_manual(values=cbp1)
ggsave(plot = p9, filename = 'figures/eval_spei/CA_density_daily_tmax.jpeg',height = 15, width  = 20, units = 'cm',device = 'jpeg')




####
#evaluation metrics for precipitation
####
library(chillR)

#points in id cause problems, remove it
new_id <- str_split(eval_daily_long$station, pattern = '[.]')
new_id <- lapply(new_id, function(x){
  x[1]
})
eval_daily_long$station <- unlist(new_id)

#boxplot of different evaluation metrics
eval_metrics <- get_eval_metrics(eval_df = eval_daily_long[eval_daily_long$variable == 'prcp',], 
                                 eval_fun = c('RMSEP', 'get_MAE',  'get_d_index', 'get_hanssen_kuipers', 'get_MCC'), 
                                 calc_summary_score = T, bigger_better = c(F, F, T, T, T),
                                 patch_fun = patch_methods[patch_methods != 'patch_mean'])

eval_metrics_long <- reshape2::melt(data = eval_metrics, id.vars = c('station', 'patch_method', 'n'), variable.name = 'metric')

#get rid of 'patch_'
eval_metrics_long$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_metrics_long$patch_method )

p10 <- eval_metrics_long %>%
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin() + facet_wrap(~metric, scales = 'free_y')  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab('Imputation Method') + 
  xlab('Score of evaluation metric') + 
  scale_fill_manual(values = cbp1_without_org)
ggsave(p10, filename = 'figures/eval_spei/CA_scores_daily_prec.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


#boxplot of different evaluation metrics
eval_metrics <- get_eval_metrics(eval_df = eval_daily_long[eval_daily_long$variable == 'tmin',], 
                                 eval_fun = c('RMSEP', 'get_MAE',  'get_NSE', 'get_S_index', 'get_coef_of_efficiency'), 
                                 calc_summary_score = T, bigger_better = c(F, F, T, T, T),
                                 patch_fun =patch_methods[patch_methods != 'patch_mean'])

eval_metrics_long <- reshape2::melt(data = eval_metrics, id.vars = c('station', 'patch_method', 'n'), variable.name = 'metric')

#get rid of 'patch_'
eval_metrics_long$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_metrics_long$patch_method )

p10_1 <- eval_metrics_long %>%
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin() + facet_wrap(~metric, scales = 'free_y')  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab('Imputation Method') + 
  xlab('Score of evaluation metric') + 
  scale_fill_manual(values = cbp1_without_org)
ggsave(p10_1, filename = 'figures/eval_spei/CA_scores_daily_tmin.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


#boxplot of different evaluation metrics
eval_metrics <- get_eval_metrics(eval_df = eval_daily_long[eval_daily_long$variable == 'tmax',], 
                                 eval_fun = c('RMSEP', 'get_MAE',  'get_NSE', 'get_S_index', 'get_coef_of_efficiency'), 
                                 calc_summary_score = T, bigger_better = c(F, F, T, T, T),
                                 patch_fun = patch_methods[patch_methods != 'patch_mean'])


eval_metrics_long <- reshape2::melt(data = eval_metrics, id.vars = c('station', 'patch_method', 'n'), variable.name = 'metric')

#get rid of 'patch_'
eval_metrics_long$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_metrics_long$patch_method )

p10_2 <- eval_metrics_long %>%
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin() + facet_wrap(~metric, scales = 'free_y')  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ylab('Imputation Method') + 
  xlab('Score of evaluation metric') + 
  scale_fill_manual(values = cbp1_without_org)
ggsave(p10_2, filename = 'figures/eval_spei/CA_scores_daily_tmax.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')



#####
#calculate monthly means and spei
#####


#drop new_na column from eval because it is specific for each variable and this causes problems
eval <- dplyr::select(eval, -'new_na')
  
#melt patching methods
eval <- reshape2::melt(eval, id.vars = c('Date', 'Year', 'Month', 'Day', 'station', 'variable'), variable.name = 'patch_method')

#dcast the variable so that this is in seperate columns again
eval <- reshape2::dcast(eval, Date + Year + Month + Day + station + patch_method ~ variable, value.var = 'value')

#summarise to monthly means / prec sum
eval_monthly <- eval %>%
  filter(patch_method != 'patch_mean') %>%
  group_by(station, Year, Month, patch_method) %>%
  summarise(tmin = mean(tmin, na.rm = T),
            tmax = mean(tmax, na.rm = T),
            prcp = sum(prcp, na.rm = T))

#add latitude info to df
#get row position of station in meta_data
eval_monthly[, 'row'] <- apply(eval_monthly, 1, function(x){
  which(meta_data$id %in% x['station'])
} )
#access latitude from meta data by row, add to monthly summary dataframe
eval_monthly[,'latitude']<- meta_data$latitude[eval_monthly$row]

#drop patch mean from levels
eval_monthly$patch_method <- factor(eval_monthly$patch_method)

#split data frame into lists
eval_monthly <- split(eval_monthly, f = list(eval_monthly$patch_method, eval_monthly$station))

#get monthly evapotranspiration 
evap_list <- lapply(eval_monthly, function(x){
  SPEI::hargreaves(Tmin = x$tmin, Tmax = x$tmax, lat = x$latitude[1], Pre = x$prcp)
})

#bring matrix to vector by rows (that is why I transposed it first)
climatic_balance <- mapply(function(x,y){
  
  #precipitation minus evapotranspiration (which was at first in matrix, melt in vector by rows)
  x$prcp - as.vector(t(y))
}, eval_monthly, evap_list) 


#calculate SPEI
spei_list <- lapply(climatic_balance, function(x) SPEI::spei(ts(x), scale = 3))

#glimpse at the spei
plot(spei_list[[1]])

spei_list <- lapply(spei_list, function(x){
  #take fitted data and bring it to vector format
  as.numeric(x$fitted)
})

#add info of month, year, station, patch method, then rbind everything
eval_monthly <- do.call(rbind, eval_monthly)
spei_list <- do.call(c, spei_list)

#add spei to eval monthly
eval_monthly$spei <- spei_list

#drop columns of row and latitude
eval_monthly <- select(eval_monthly, -all_of(c('row', 'latitude')))

#dcast and melt eval_monhtly to make original a seperate column
eval_monthly <- reshape2::melt(eval_monthly, id.vars = c('station', 'Year', 'Month', 'patch_method'))

eval_monthly <- reshape2::dcast(eval_monthly, station + Year + Month + variable ~ patch_method, value.var = 'value')

eval_monthly <- reshape2::melt(eval_monthly, id.vars= c('station', 'Year', 'Month', 'variable', 'original'), variable.name = 'patch_method')

library(ggplot2)

#calculate r2 and add it to the plot
r2 <- eval_monthly %>%
  filter(is.na(value) == F, is.infinite(value) == F, is.na(original) == F, is.infinite(original) == F) %>%
  group_by(patch_method, variable) %>%
  summarise(r2 = cor(value, original,use = "pairwise.complete.obs")^2,
            rmse = chillR::RMSEP(predicted = value, observed = original, na.rm = T))


p11 <- eval_monthly %>%
  filter(variable == 'spei', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'spei',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), '\nRMSE = ', round(rmse, digits = 2), sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted SPEI') + 
  xlab('Observed SPEI') +
  facet_wrap(~patch_method)

p12 <- eval_monthly %>%
  filter(variable == 'prcp', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'prcp',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), '\nRMSE = ', round(rmse, digits = 2), sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted Monthly Precipitation Sum [mm]') + 
  xlab('Observed Monthly Precipitation Sum [mm]') +
  facet_wrap(~patch_method)

p13 <- eval_monthly %>%
  filter(variable == 'tmin', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'tmin',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), '\nRMSE = ', round(rmse, digits = 2), sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted Monthly Mean Minimum Temperature [°C]') + 
  xlab('Observed Monthly Mean Minimum Temperature [°C]') +
  facet_wrap(~patch_method)

p14 <- eval_monthly %>%
  filter(variable == 'tmax', is.na(value) == F, is.na(original) == F) %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') +
  geom_label(data = r2[r2$variable == 'tmax',], 
             aes(x = Inf, y = -Inf, 
                 label = paste("R^2 = ", round(r2,digits = 2), '\nRMSE = ', round(rmse, digits = 2), sep = " ")),
             hjust = 1, vjust = 0)+
  ylab('Predicted Monthly Mean Maximum Temperature [°C]') + 
  xlab('Observed Monthly Mean Maximum Temperature [°C]') +
  facet_wrap(~patch_method)

ggsave(p11, filename = 'figures/eval_spei/CA_qq_monthly_spei.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')
ggsave(p12, filename = 'figures/eval_spei/CA_qq_monthly_prcp.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')
ggsave(p13, filename = 'figures/eval_spei/CA_qq_monthly_tmin.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')
ggsave(p14, filename = 'figures/eval_spei/CA_qq_monthly_tmax.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')



### boxplot of observed (cconfidence interval) and predicted precipitation sums per month
eval_monthly2 <- reshape2::dcast(eval_monthly, formula = station + Year + Month + variable + original ~ patch_method, value.var = 'value')
eval_monthly2 <- reshape2::melt(eval_monthly2, id.vars = c('station', 'Year', 'Month', 'variable'), variable.name = 'patch_method')
eval_monthly2$patch_method <- gsub(pattern = 'patch_', replacement = '', x = eval_monthly2$patch_method)

org_ci <- eval_monthly2 %>%
  filter(patch_method == 'original', is.na(value) == F, is.infinite(value) == F) %>%
  group_by(Month, variable) %>%
  summarise(lower = as.numeric(quantile(value, probs = 0.05, na.rm = T)),
            upper = as.numeric(quantile(value, probs = 0.95, na.rm = T)))
org_ci$patch_method <- 'original'



#plot imputed precipitation sum against precipitation sum confidence interval observed
p15 <- ggplot() + 
  geom_blank(data= eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'tmin',], aes(x = as.factor(Month) , y =  value)) + 
  geom_ribbon(data = org_ci[org_ci$variable == 'tmin',], 
              aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'tmin',], 
               aes(x = as.factor(Month), y = value, fill = patch_method))+
  scale_fill_manual(values=cbp1_without_org)
ggsave(p15, filename = 'figures/eval_spei/CA_monthly_tmin_boxplot.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')

p16 <- ggplot() + 
  geom_blank(data= eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'prcp',], aes(x = as.factor(Month) , y =  value)) + 
  geom_ribbon(data = org_ci[org_ci$variable == 'prcp',], 
              aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'prcp',], 
               aes(x = as.factor(Month), y = value, fill = patch_method))+
  scale_fill_manual(values=cbp1_without_org)
ggsave(p16, filename = 'figures/eval_spei/CA_monthly_prcp_boxplot.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


p17 <- ggplot() + 
  geom_blank(data= eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'spei',], aes(x = as.factor(Month) , y =  value)) + 
  geom_ribbon(data = org_ci[org_ci$variable == 'spei',], 
              aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = eval_monthly2[eval_monthly2$patch_method != 'original' & eval_monthly2$variable == 'spei',], 
               aes(x = as.factor(Month), y = value, fill = patch_method))+
  scale_fill_manual(values=cbp1_without_org)
ggsave(p17, filename = 'figures/eval_spei/CA_monthly_spei_boxplot.jpeg', height = 15, width = 20, units = 'cm', device = 'jpeg')


#the methods preserve the monthly means well....
#maybe just deleting random days is not enough? Or it just shows, that the choice of methods doesn't matter this much
#I sense danger comming
