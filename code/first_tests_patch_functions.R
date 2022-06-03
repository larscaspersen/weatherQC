#this script is to carry out analysis of the patch function file

library(ggplot2)
library(chillR)


#read all patching functions
source('code/patching_function.R')

####
#input data preparation
####


prcp <- read.table('data/Salado_prcp.dat', header = T)
tmin <- read.table('data/Salado_tmin.dat', header = T)
tmax <- read.table('data/Salado_tmax.dat', header = T)
meta_data <- read.table('data/Salado_metadata.dat', header = T)


#how should the input data look like?
#one table for one variable
#target and donor are listed together in one table
#identify target by name
#metadata gives coordinates of each station, have same names as in variable data
#assume that the order of the original data is the same as in the metadata
#assume that first column is used for Date

#change names to latitude and longitide in metadata
colnames(meta_data)[3:4] <- c('longitude', 'latitude')

#make id of station a character
meta_data$omm_id <- paste0('id_', meta_data$omm_id)

#identified in metadata needs to correspond to colnames in weather data
colnames(tmin)[2:18] <- meta_data$omm_id
colnames(tmax)[2:18] <- meta_data$omm_id
colnames(prcp)[2:18] <- meta_data$omm_id

#change column name to 'id', 'name' and 'elevation'
colnames(meta_data)[c(1,2,5)] <- c('id', 'name', 'elevation')
write.csv(meta_data, file = 'data/Salado_metadata.csv', row.names = F)


#change 'date' to 'Date'
colnames(tmin)[1] <- colnames(tmax)[1] <- colnames(prcp)[1] <- 'Date'

#add columns of Day, Month, Year to tmin

tmin$Year <- format(as.Date(tmin$Date), format = '%Y')
tmin$Month <- format(as.Date(tmin$Date), format = '%m')
tmin$Day <- format(as.Date(tmin$Date), format = '%d')

tmax$Year <- format(as.Date(tmax$Date), format = '%Y')
tmax$Month <- format(as.Date(tmax$Date), format = '%m')
tmax$Day <- format(as.Date(tmax$Date), format = '%d')

prcp$Year <- format(as.Date(prcp$Date), format = '%Y')
prcp$Month <- format(as.Date(prcp$Date), format = '%m')
prcp$Day <- format(as.Date(prcp$Date), format = '%d')

#write data in file so that I dont have to adjust the format everytime
write.csv(tmin, file = 'data/salado_tmin.csv', row.names = F)
write.csv(tmax, file = 'data/salado_tmax.csv', row.names = F)
write.csv(prcp, file = 'data/salado_prcp.csv', row.names = F)

######
#carry out patching
######

#set which patch methods to use
patch_methods <- c("patch_mean", "patch_normal_ratio", "patch_normal_ratio", 'patch_mice', 'patch_amelia', 'patch_climatol')

#set precipitation threshold
prcp_threshold <- 1

#set which weather variable to investigate
weather <- prcp

#set every case which had lower precipitation as the trheshold to zero
for(station in meta_data$id){
  weather[weather[,station] < prcp_threshold & !is.na(weather[,station]),station] <- 0
}

#set the missingness, which should be investigated
p_missing <- 0.3

#indicate if function either patches the whole data frame or if it patches one column after another
method_patches_everything = c(F, F, F, T, T, T)

#define additional arguments needed for the function call, needs to be of same length as patch methods
additional_args <- list(list(n_donors = 5),
                        list(n_donors = 5, weight_type = 'ordinary'),
                        list(n_donors = 5, weight_type = 'correlation'),
                        list(rain_data = T, prcp_threshold = prcp_threshold, n.impute = 5, max.iter = 5),
                        list(rain_data = T, prcp_threshold = prcp_threshold, n.impute = 5),
                        NA)

eval <- get_eval_one_station(weather = weather, target = meta_data$id, 
                             patch_methods = patch_methods, p_missing = p_missing, 
                             additional_args = additional_args,
                             method_patches_everything = method_patches_everything,
                             return_data = 'evaluation_period')

#bring to long format, to more easily adjust values below precipitation threshold to zero
eval_long <- reshape2::melt(eval, id.vars = c('Date', 'Year', 'Month', 'Day', 'station', 'original', 'new_na'), variable.name = 'patch_method')
eval_long$value[eval_long$value < prcp_threshold] <- 0
eval_long$original[eval_long$original < prcp_threshold] <- 0
# #bring back to long format
#eval <- reshape2::dcast(eval_long, formula = Date + Year + Month + Day + station ~ variable, value.var = 'value' )

sum(eval_long$new_na) / nrow(eval_long)

#calculate evaluation metrics

#only evaluate for newly imputed data
eval_metrics <- get_eval_metrics(eval_df = eval_long[eval_long$new_na,], 
                                 eval_fun = c('RMSEP', 'get_MAE',  'get_d_index', 'get_hanssen_kuipers', 'get_MCC'), 
                                 calc_summary_score = T, bigger_better = c(F, F, T, T, T),
                                 patch_fun = patch_methods)

#bring eval metrics to long format
eval_metrics_long <- reshape2::melt(data = eval_metrics, id.vars = c('station', 'patch_method', 'n'), variable.name = 'metric')

theme_set(theme_bw(16))

#plot evaluation metrics of daily precipitation
eval_metrics_long %>%
  ggplot(aes(x = patch_method, y = value, fill = patch_method)) + 
  geom_violin() + facet_wrap(~metric, scales = 'free_y')  +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_fill_manual(values = cbp1_without_org)



#colourblind friendly scales;
#with grey
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1_without_org <- c("#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#with black
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp2_without_org <- c("#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#calculate coefficient of determination
r2 <- eval_long %>%
  filter(new_na == T) %>%
  group_by(patch_method) %>%
  summarise(r2 = cor(value, original)^2)

#plot qq-plot of daily precipitation
eval_long %>%
  filter(new_na == T) %>%
  ggplot(aes(x = original, y = value)) + 
  geom_point(alpha = 0.1)+
  geom_abline(slope = 1, linetype = 'dashed', color = 'red')+
  geom_label(data = r2, 
             aes(x = Inf, y = Inf, 
                 label = paste("R2 = ", round(r2,digits = 2), sep = " ")),
             hjust = 1, vjust = 1)+
  facet_wrap(~patch_method)


eval_long2 <- reshape2::melt(eval, id.vars = c('Date', 'Year', 'Month', 'Day', 'station', 'new_na'))

#check the differences in density plot
eval_long2 %>%
  filter(new_na == T) %>%
  ggplot(aes(value+1, col = variable)) + stat_ecdf(geom = "step") + 
  coord_trans(x="log2") + 
  scale_colour_manual(values=cbp2)



#check for aggregated variables
monthly_summarised <- eval_long2 %>%
  group_by(station, Month, Year, variable) %>%
  summarise(mean = mean(value, na.rm = T), sd = sd(value, na.rm = T),
            sum = sum(value, na.rm = T))


#extract the original means and prepare them to be a ribbon
org_ci <- monthly_summarised %>%
  filter(variable == 'original') %>%
  group_by(Month) %>%
  summarise(lower_sum = as.numeric(quantile(sum, probs = 0.05, na.rm = T)),
            upper_sum = as.numeric(quantile(sum, probs = 0.95, na.rm = T)),
            lower_mean = as.numeric(quantile(mean, probs = 0.05, na.rm = T)),
            upper_mean = as.numeric(quantile(mean, probs = 0.95, na.rm = T)),
            lower_sd = as.numeric(quantile(sd, probs = 0.05, na.rm = T)),
            upper_sd = as.numeric(quantile(sd, probs = 0.95, na.rm = T)))
org_ci$patch_method <- 'original'



#plot imputed precipitation sum against precipitation sum confidence interval observed
ggplot() + 
  geom_blank(data= monthly_summarised, aes(x = Month , y =  sum)) + 
  geom_ribbon(data = org_ci, aes(ymin = lower_sum, ymax = upper_sum, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = monthly_summarised[monthly_summarised$variable != 'original',], 
               aes(x = Month, y = sum, fill = variable))+
  scale_fill_manual(values=cbp1_without_org)

ggplot() + 
  geom_blank(data= monthly_summarised, aes(x = Month , y =  sd)) + 
  geom_ribbon(data = org_ci, aes(ymin = lower_sd, ymax = upper_sd, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = monthly_summarised[monthly_summarised$variable != 'original',], 
               aes(x = Month, y = sd, fill = variable))+
  scale_fill_manual(values=cbp1_without_org)

ggplot() + 
  geom_blank(data= monthly_summarised, aes(x = Month , y =  mean)) + 
  geom_ribbon(data = org_ci, aes(ymin = lower_mean, ymax = upper_mean, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = monthly_summarised[monthly_summarised$variable != 'original',], 
               aes(x = Month, y = mean, fill = variable))+
  scale_fill_manual(values=cbp1_without_org)

#reorganize the monthly summarised data so that it can be plotted as qqplot
mon_sum_long <-  reshape2::melt(monthly_summarised, measure.vars = c('mean', 'sd', 'sum'), variable.name = 'sum_metric')
mon_sum_long <-  reshape2::dcast(mon_sum_long, station + Month + Year + sum_metric ~ variable, value.var = 'value')
mon_sum_long <-  reshape2::melt(mon_sum_long, id.vars = c('station', 'Year', 'Month', 'original', 'sum_metric'))




#scatterplot of predicted and observed monthly sums

#calc coefficient of determination
r2 <- mon_sum_long %>%
  group_by(variable, sum_metric) %>%
  summarise(r2 = cor(value, original, use = 'pairwise.complete.obs')^2)

#make scatterplot of monthly sums, with coefficient of determination
mon_sum_long %>%
  filter(sum_metric == 'sum') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') + 
  geom_label(data = r2[r2$sum_metric == 'sum', ], 
             aes(x = Inf, y = Inf, 
                 label = paste("R2 = ", round(r2,digits = 2), sep = " ")),
             hjust = 1, vjust = 1)+
  facet_wrap(~variable)

mon_sum_long %>%
  filter(sum_metric == 'sd') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') + 
  geom_label(data = r2[r2$sum_metric == 'sd', ], 
             aes(x = Inf, y = Inf, 
                 label = paste("R2 = ", round(r2,digits = 2), sep = " ")),
             hjust = 1, vjust = 1)+
  facet_wrap(~variable)

mon_sum_long %>%
  filter(sum_metric == 'mean') %>%
  ggplot(aes(x = original, y = value)) + geom_point(alpha = 0.2) + 
  geom_abline(slope = 1, linetype = 'dashed', color = 'red') + 
  geom_label(data = r2[r2$sum_metric == 'mean', ], 
             aes(x = Inf, y = Inf, 
                 label = paste("R2 = ", round(r2,digits = 2), sep = " ")),
             hjust = 1, vjust = 1)+
  facet_wrap(~variable)

#only few difference in monthly data: maybe it is because the missingness is set low?


#annual precipitation sum
anually_summarised <-eval_long2 %>%
  group_by(station, Year, variable) %>%
  summarise(mean = mean(value, na.rm = T), sd = sd(value, na.rm = T),
            sum = sum(value, na.rm = T))


#extract the original means and prepare them to be a ribbon
#this doesn't work when there is no grouping element. should just calculate the boundaries manually
org_ci_annual <- data.frame(lower_sum = as.numeric(quantile(anually_summarised[anually_summarised$variable == 'original','sum'], probs  = 0.05, na.rm = T)),
                            upper_sum = as.numeric(quantile(anually_summarised[anually_summarised$variable == 'original','sum'], probs  = 0.95, na.rm = T)),
                            lower_mean = as.numeric(quantile(anually_summarised[anually_summarised$variable == 'original','mean'], probs  = 0.05, na.rm = T)),
                            upper_mean = as.numeric(quantile(anually_summarised[anually_summarised$variable == 'original','mean'], probs  = 0.95, na.rm = T)),
                            lower_sd = as.numeric(quantile(anually_summarised[anually_summarised$variable == 'original','sd'], probs  = 0.05, na.rm = T)),
                            upper_sd = as.numeric(quantile(anually_summarised[anually_summarised$variable == 'original','sd'], probs  = 0.95, na.rm = T)))

org_ci_annual$patch_method <- 'original'


#plot imputed precipitation sum against precipitation sum confidence interval observed
ggplot() + 
  geom_blank(data= anually_summarised, aes(x = 1, y =  sum)) + 
  geom_rect(data = org_ci_annual, aes(ymin = lower_sum, ymax = upper_sum, xmin = 0.5, xmax = 1.5), alpha = 0.3, fill = 'grey') + 
  geom_boxplot(data = anually_summarised[anually_summarised$variable != 'original',], 
               aes(x = 1, y = sum, fill = variable))+
  scale_fill_manual(values=cbp1_without_org)

ggplot() + 
  geom_blank(data= anually_summarised, aes(x = 1, y =  sd)) + 
  geom_rect(data = org_ci_annual, aes(ymin = lower_sd, ymax = upper_sd, xmin = 0.5, xmax = 1.5), alpha = 0.3, fill = 'grey') + 
  geom_boxplot(data = anually_summarised[anually_summarised$variable != 'original',], 
               aes(x = 1, y = sd, fill = variable))+
  scale_fill_manual(values=cbp1_without_org)

ggplot() + 
  geom_blank(data= anually_summarised, aes(x = 1, y =  mean)) + 
  geom_rect(data = org_ci_annual, aes(ymin = lower_mean, ymax = upper_mean, xmin = 0.5, xmax = 1.5), alpha = 0.3, fill = 'grey') + 
  geom_boxplot(data = anually_summarised[anually_summarised$variable != 'original',], 
               aes(x = 1, y = mean, fill = variable))+
  scale_fill_manual(values=cbp1_without_org)





















#bring data in long format
eval_long <- melt(eval, id.vars = c('Date', 'Year', 'Month', 'Day', 'station', 'original'), variable.name = 'patch_method')

#clean data which is below precipitation limit
eval_long$value[eval_long$value < prcp_threshold] <- 0


#arguments to get evaluation data


p_missing <- 0.2

naniar::vis_miss(weather[,c('id_87544', 'id_87468', 'id_87540')]) + 
  geom_linerange(aes(x = 1, ymin = period1[1], ymax = period1[2]),
                 color = "red", size = 1) +
  geom_linerange(aes(x = 2, ymin = period2[1], ymax = period2[2]),
                 color = "red", size = 1) +
  geom_linerange(aes(x = 3, ymin = period3[1], ymax = period3[2]),
                 color = "red", size = 1) +
  #coord_cartesian(ylim = c(2500, 0))
  #coord_cartesian(ylim = c(11000, 10000))
  #coord_cartesian(ylim = c(15000, 12000))
  coord_cartesian(ylim = c(20000, 18000))

longest_coverage <- na.contiguous(weather[,c('id_87544')])
period1 <- tsp(longest_coverage)[1:2]

longest_coverage <- na.contiguous(weather[,c('id_87468')])
period2 <- tsp(longest_coverage)[1:2]

longest_coverage <- na.contiguous(weather[,c('id_87540')])
period3 <- tsp(longest_coverage)[1:2]

rect_df <- data.frame(xmin = c('id_87544', 'id_87468', 'id_87540'), xmax = c('id_87468', 'id_87540', 'id_87540'),
                      ymin = c(period1[1], period2[1], period3[1]), ymax = c(period2[2], period2[2], period3[2]))




eval <- get_eval_one_station(weather = weather, target = c('id_87544', 'id_87468', 'id_87540'), 
                             patch_methods = patch_methods, p_missing = p_missing, 
                             additional_args = additional_args,
                             method_patches_everything = method_patches_everything,
                             return_data = 'data/')


######
#USE EVALUATION FUNCTIONS
######




#calculate evaluation metrics
#eval_fun: needs to be exactly the function name of the evaluation function, evaluation function should only take predicted and observed data as an input argument
#          --> any custom function using these two arguments can be used
#calc_summary_score: calculate a weighted summary score of all evaluation metrics, evaluation metrics are rescaled so that max(matri) == 1
#                    this function allows also custom weightening of the evaluation metrics
eval_metrics <- get_eval_metrics(eval_df = eval, 
                                 eval_fun = c('RPIQ','RMSEP', 'get_d_index', 'get_hansen_kuipers', 'get_MCC', 'get_hit_score'), 
                                 calc_summary_score = F, 
                                 patch_fun = patch_methods)
#this bad boy should be able to handle any evaluation method, aslong it only takes
#predicted as first argument and observed as second argument

RPIQ
quantile


eval_long %>%
  filter(station == 'id_87544', patch_method ==  'patch_mean') %>%
  summarise(quan = quantile(original))

as.numeric(quantile(observed, na.rm = na.rm)[4] - quantile(observed, 
                                                           na.rm = na.rm)[2])/sqrt(sum((observed - predicted)^2, 
                                                                                       na.rm = na.rm)/length(which(!is.na(observed - predicted))))

#####
#PLOTTING
#####


#take the method with the lowest score

eval_metrics %>%
  ggplot(aes(x = patch_method, y = RMSEP)) +
  geom_boxplot()

eval_metrics %>%
  ggplot(aes(x = patch_method, y = RPIQ)) +
  geom_boxplot()


#make a qqplot

patch_methods <- make.unique(patch_methods, sep = '_')

eval_long <- melt(eval, measure.vars = patch_methods, variable.name = 'patch_method')

eval_long %>%
  ggplot(aes(x = value, y = original)) + 
  geom_point(alpha = 0.1) + 
  geom_abline(slope = 1, linetype = 'dashed', col = 'red') +
  facet_grid(~patch_method)


#for density plot I also need original data in long format
eval_long2 <- melt(eval, measure.vars = c('original', patch_methods))


#check the differences in density plot
ggplot(eval_long2, aes(value, col = variable)) + stat_ecdf(geom = "step") +
  scale_color_manual(values = c('black', "#00AFBB", "#E7B800", "#FC4E07"))+
  scale_linetype_manual(values=c("dotted", 'solid', 'solid', 'solid'))
#looks like all methods overestimate low temperatures







#####
#test calculating aggregated outcomes
####

patch_methods <- c("patch_mean", "patch_normal_ratio", "patch_normal_ratio")

#get evaluation data of all stations
eval_list <- lapply(meta_data$id, function(x){
  
  #get evaluation data
  get_eval_one_station(weather = tmin,target = x, 
                       patch_methods = patch_methods,
                       additional_args = additional_args,
                       return_whole_data = T,
                       p_missing = 0.3)
  
})

#bind by rows
eval <- data.table::rbindlist(eval_list)

patch_methods <- make.unique(patch_methods, sep = '_')

#calculate mean value per Month
eval_long <- melt(eval, measure.vars = c('original',patch_methods), variable.name = 'patch_method')

Monthly_means <- aggregate(eval_long$value, by = list(patch_method = eval_long$patch_method,
                                                      Year = eval_long$Year,
                                                      Month = eval_long$Month, 
                                                      station = eval_long$station), FUN = mean)

Monthly_sd <- aggregate(eval_long$value, by = list(patch_method = eval_long$patch_method,
                                                   Year = eval_long$Year,
                                                   Month = eval_long$Month, 
                                                   station = eval_long$station), FUN = sd)

#extract the original means and prepare them to be a ribbon
org_ci <- Monthly_means %>%
  filter(patch_method == 'original') %>%
  group_by(Month) %>%
  summarise(lower = as.numeric(quantile(x, probs = 0.05)),
            upper = as.numeric(quantile(x, probs = 0.95)))
org_ci$patch_method <- 'original'



#this works :))))))))))
ggplot() + 
  geom_blank(data= Monthly_means, aes(x = Month , y =  x)) + 
  geom_ribbon(data = org_ci, aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = Monthly_means[Monthly_means$patch_method != 'original',], 
               aes(x = Month, y = x, fill = patch_method))



org_ci_sd <- Monthly_sd %>%
  filter(patch_method == 'original') %>%
  group_by(Month) %>%
  summarise(lower = as.numeric(quantile(x, probs = 0.05)),
            upper = as.numeric(quantile(x, probs = 0.95)))
org_ci_sd$patch_method <- 'original'

ggplot() + 
  geom_blank(data= Monthly_sd, aes(x = Month , y =  x)) + 
  geom_ribbon(data = org_ci_sd, aes(ymin = lower, ymax = upper, x = as.numeric(Month)), alpha = 0.3) + 
  geom_boxplot(data = Monthly_sd[Monthly_sd$patch_method != 'original',], 
               aes(x = Month, y = x, fill = patch_method))





#calculate scores on Monthly means

#bring back to long format
eval.Monthly <- dcast(data = Monthly_means,formula = Year + Month + station~patch_method, value.var = 'x')


#metrics which can go below 0 cause problems!

eval_metrics <- get_eval_metrics(eval_df = eval.Monthly, eval_fun = c('RPIQ','RMSEP', 'get_MAE', 'cor'), 
                                 calc_summary_score = T, bigger_better = c(T, F, F, T), 
                                 patch_fun = patch_methods)

#outcome of Monthly means
ggplot(eval_metrics, aes(x = patch_method, y= score)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))



#bring back to long format
eval.sd <- dcast(data = Monthly_sd,formula = Year + Month + station~patch_method, value.var = 'x')



eval_metrics.sd <- get_eval_metrics(eval_df = eval.sd, eval_fun = c('RPIQ','RMSEP', 'get_MAE', 'cor'), 
                                    calc_summary_score = T, bigger_better = c(T, F, F, T), 
                                    patch_fun = patch_methods)

#outcome of Monthly means
ggplot(eval_metrics.sd, aes(x = patch_method, y= score)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))



