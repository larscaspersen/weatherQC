library(dplyr)

#####
# patching functions
#####


#take five closest stations, calculate mean
#(alternatively: take for each missing Day the five closest stations with observations)
patch_mean <- function(weather, target, meta_data, n_donors = 5, max_dist = 150){
  
  #extract lon and lat of target station
  target_lon <- meta_data[meta_data$id == target, 'longitude']
  target_lat <- meta_data[meta_data$id == target, 'latitude']
  
  #calculate distances between stations, choose the n closesest ones
  meta_data$distance <- round(sp::spDistsN1(as.matrix(meta_data[, c("longitude", "latitude")]),
                                               c(target_lon, target_lat), longlat = TRUE), 2)
  
  #sort to by increasing distance
  meta_data <- meta_data[order(meta_data$distance),]
  
  #drop stations which are further than the maxium distance to include
  meta_data <- meta_data[meta_data$distance <= max_dist,]
  
  #if there are less then three stations (excluding target station remaining), then sent a warning and stop
  if(nrow(meta_data) < 4){
    warning('There are less than 3 auxiliary stations within the specified maximum distance. Consider increasing the max_dist parameter or choosing a different patching function')
    return(weather[,target])
  }
  
  #order weather by distance
  weather <- weather[, meta_data$id]
  
  #get mean of closest stations with readings
  means <- apply(as.matrix(weather), MARGIN = 1, function(x){
    
    #amount of non-NA observations for that day of the closest neighbours
    y <- sum(!is.na(x)[2:(2+n_donors-1)])
    
    #amount of extra-columns to include to get n_donros non-NA readings
    i <- 0
    
    #only increase i if there are stations available
    if(length(x) > 2+n_donors-1+i){
      #increase i until we either have enough readings or we run out of columns
      while(y <= n_donors-1){
        i <- i +1
        y <- sum(!is.na(x)[2:(2+n_donors-1+i)])
        if(2+n_donors-1+i >= ncol(weather)) break()
      }
    }
    
    
    #return the mean
    return(mean(x[2:(2+n_donors-1+i)], na.rm = T))
  })

  #identify rows of missing data in target
  row_na <- which(is.na(weather[,target]))
  
  #fill gaps with means
  weather[row_na, target] <- means[row_na]
  
  return(weather[,target])
}



####
# Normal Ratio
####


patch_normal_ratio <- function(weather, target, meta_data, n_donors){
  
  #extract lon and lat of target station
  target_lon <- meta_data[meta_data$id == target, 'longitude']
  target_lat <- meta_data[meta_data$id == target, 'latitude']
  
  #calculate distances between stations, choose the n closesest ones
  meta_data$distance <- round(sp::spDistsN1(as.matrix(meta_data[, c("longitude", "latitude")]),
                                            c(target_lon, target_lat), longlat = TRUE), 2)
  
  #sort to by increasing distance
  meta_data <- meta_data[order(meta_data$distance),]
  

  #get correlation coefficient 
  corr_weather <- weather %>%
    dplyr::select(-c('Date', 'Year', 'Month', 'Day')) %>%
    cor(use = "pairwise.complete.obs")
  
  #get cases in which we have observation
  x <- weather %>%
    dplyr::select(-c('Date', 'Year', 'Month', 'Day')) %>%
    is.na()
  x <- !x
  
  #get sum of pairwise observations
  n_pairwise_obs <- t(x) %*% x
    
  
  #identify rows of missing data in target
  row_na <- which(is.na(weather[,target]))
  
  #for each missing Day in target, return names of weather stations with data
  stat_obs <- lapply(row_na, FUN = function(x){
    return(list(row = x, sation_id = colnames(weather)[!is.na(weather[x,])]))
  })
  
  
  
  
  #define function to calculate NR for stations with reading at that time 
  nr_of_closest_stations <- function(weather, stations_observed, meta_data, 
                                     target,
                                     corr_weather = NA, 
                                     n_pairwise_obs = NA,
                                     n_donors = 5, weight_type = 'ordinary'){
    
    #extract the information of target row and the closest stations by 
    #increasing distance with readings for the target row
    target_row <- stations_observed$row
    closest_station <- stations_observed$sation_id
    
    #select the closest stations
    if(sum(meta_data$id %in% closest_station) == 0){
      #in case no station has a reading at that day, NA remains and a warning will be sent
      impute <- NA
      warning(paste0('For weather station ', target, ' in  row ', target_row, 'there was no neighbour with a reading. Gap remains unfilled'))
    
    #in case of at least one neighbour with a suitable reading, continue with the weightening
    } else {
      if(sum(meta_data$id %in% closest_station) < n_donors){
        
        closest_n <- meta_data$id[which(meta_data$id %in% closest_station)]
      } else {
        #take the n stations with observations, which are the closest
        closest_n <- meta_data$id[which(meta_data$id %in% closest_station)[1:n_donors]]
      }
      
      #get weights
      weights <- lapply(closest_n, function(x){
        r_squared <- corr_weather[target, x]^2
        weight <- r_squared*((n_pairwise_obs[target, x] - 2)/(1 - r_squared))
        return(weight)
      })
      
      #make weights to vector
      weights <- unlist(weights)
      
      impute <- sum(weather[target_row, closest_n] * weights) / sum(weights)
        


    } #end of case where there are enough neighbours
    
    return(impute)
  }
  

  #calculate for each row of na, the normal ratio of the closest stations
  impute <- lapply(stat_obs, FUN = function(x){
    nr_of_closest_stations(weather = weather, stations_observed = x,
                           corr_weather = corr_weather, n_pairwise_obs = n_pairwise_obs,
                           meta_data = meta_data, n_donors = n_donors, weight_type = weight_type,
                           target = target)
  })
  

  weather[row_na, target] <- unlist(impute)
  
  return(weather[,target])


}



#need to install methodsPCA first
patch_pca  <- function(weather, meta_data, target, method = 'nipals', nPcs = 2){
  
  if (!require("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")
    BiocManager::install("pcaMethods")
  }

  
  
  pc <-  pcaMethods::pca(weather[,meta_data$id],method = method, nPcs = nPcs )

  return(pcaMethods::completeObs(pc))
}

patch_vector_sampling <- function(weather, target, meta_data, k = 5){
  
  library(data.table)
  
  weather <- data.table(tmin)
  #copy of weather
  
  #copy of original weather, which won't be changed
  org_weather <- select(weather, -all_of(c('Year', 'Month', 'Day', 'Date')))
  
  #copy of weather,where gaps will be filled
  out <- select(weather, -all_of(c('Year', 'Month', 'Day', 'Date')))
  
  #vector saying where is missing data
  org_nanind <- is.na(out)
  
  #repeat loop several times
  for(k in 1:2000){
    #check for each iteration where are still gaps in data
    nanind <- is.na(out)
    dataind <- !(nanind)
    #give some info on the progress######
    
    
    #initialise some objects
    dout <- D <- Dmean <- sort_k_ind <- C <- Cdata <- Dsort <- Cest <- NULL
    
    #if there are still gaps
    if(any(nanind)){
      
      pb = txtProgressBar(min = 0, max = nrow(org_weather), initial = 0) 


      for(i in 1:nrow(org_weather)){
        
      #  print(i)
        
        #if row contains any nas
        if(any(nanind[i,])){
          
          #target row
          dout <- out[i,]
          
          #distance of all lines to dout
          D <- abs(sweep(org_weather, MARGIN = 2, as.numeric(dout))^2)
          
          #mean distance per station
          Dmean <- colMeans(D, na.rm = T)
          #make Dmean the size of D
          Dmean <- matrix(Dmean,nrow = nrow(D), ncol = ncol(D), byrow = T)
          
          #assign mean where distance is missing, only for columns relevant for the row
          D[(org_nanind & dataind[i,])]  <- Dmean[(org_nanind & dataind[i,])]
          
          #get row means of distnace
          D <- rowMeans(D, na.rm = T)
          
          #add a tiny number (dont know why, maybe to prevent zero distances)
          D <- D + 1e-6
          
          
          #check if original data gives information for missing columns of that line, if not set distance to infinity
          D[rowSums(is.na(org_weather[,nanind[i,],with = FALSE])) == sum(nanind[i,])] <- Inf
          
          
          #get index of ordered D
          sort_k_ind <- order(D)[1:k]
          
          #matrix of weight, same weight per line, different weight per row
          Dsort <- matrix(sort(D)[1:k], nrow =k, ncol = ncol(org_weather), byrow = F) 
          
          #take candidates observation (of the rows with the smallest distance)
          C <- org_weather[sort_k_ind,]
          
          #dataframe saying where observations are present in candidates
          Cdata <- !is.na(C)
          
          #estimation of missing data, use weighted mean of candidates
          Cest <- colSums(C/Dsort) / colSums((1/Dsort) * Cdata)
          
          cols <- colnames(out)[nanind[i,]]
          
          #assign estimated values
          set(out,i = i, j = cols, value = as.list(Cest[nanind[i,]]))

        }
        setTxtProgressBar(pb,i)
      }
        
    }
    
  } #end of loop for repititions
  
  
  
  target <- meta_data$id[1]
  row <- which(is.na(weather[,target]))[1]
  other_stations <- meta_data$id[meta_data$id != target]
  
  #identify which stations have an observation for that day
  
  #row to be patched
  dout <- weather[row,meta_data$id]
  
  #distance of every other line to dout
  D <- (sweep(weather[-row, meta_data$id], MARGIN = 2, unlist(dout)))^2
  
  df <- data.frame(a=1:3, b = 1:3, c = 1:3)
  targ <- c(NA, 1, 2)
  
  sweep(df, MARGIN = 2, targ)
  
  df - t(targ)
  
  #get mean distance
  Dmean <- colMeans(D,na.rm = T)
  
  s <- names(weather[row,other_stations])[which(!is.na(weather[row,other_stations]))]
  pool <- weather[-row, s]
  
  #drop rows from pool which contain only na
  pool <- pool[rowSums(is.na(pool)) != ncol(pool), ]
  
  #calculate distance of pool to target row
  dist <- (pool - unlist(weather[row,s]))^2
  
  
  #complete set of these stations (without the target row)
  mean_dist <- colMeans((na.omit(pool) - unlist(weather[row,s]))^2)
  #if there are NA's in the pool, fill with mean distance of the specific station
  for(i in s){
    dist[is.na(dist[,i]),i] <- mean_dist[i]
  }
  
  D <- rowSums(dist)
  candidate_rows <- order(D,decreasing = F)[1:k]
  
  
  
  pool[candidate_rows,]

  #get for each station, the mean distance to weather[row, s]
  
  ####temp
}



patch_idw <- function(weather, meta_data, phi = 2, target){
  
  ###temp
  #make points spatial
  p1 <- sp::SpatialPoints(meta_data[, c("longitude", "latitude")])
  sp::proj4string(p1) <- "+proj=longlat"
  #calculate distances between all point combinations
  dist <- sp::spDists(p1, p1)
  
  #calculate weights
  weights <- apply(dist, MARGIN = 2, function(x) x^(-phi))
  
  #set weights for own station to zero
  weights[is.infinite(weights)] <- 0
  
  #replace NAs with zeros in weather, so that they dont bother in matrix multiplication
  weather_m <- weather[,meta_data$id]
  weather_full <- weather_m
  weather_full[is.na(weather_full)] <- 0
  
  #do matrix multiplication
  m1 <- (as.matrix(weather_full) %*% weights)
  
  m2 <- apply(m1, 1, function(x){
    x / colSums(weights)
  })

  m2 <- t(m2)
  colnames(m2) <- meta_data$id
  
  weather_m[is.na(as.matrix(weather_m))] <- m2[is.na(as.matrix(weather_m))]
  return(weather_m)

  ###temp
  
  # #extract lon and lat of target station
  # target_lon <- meta_data[meta_data$id == target, 'longitude']
  # target_lat <- meta_data[meta_data$id == target, 'latitude']
  # 
  # #calculate distances between stations, choose the n closesest ones
  # meta_data$distance <- round(sp::spDistsN1(as.matrix(meta_data[, c("longitude", "latitude")]),
  #                                           c(target_lon, target_lat), longlat = TRUE), 2)
  # 
  # #calculate inverse distance weight
  # meta_data$weight <- meta_data$distance^(-phi)
  # 
  # #put in extra object
  # weight_df <- meta_data[meta_data$id != target,]
  # 
  # #bring weather to extra object, fill every NA with 0 for matrix multiplication
  # weather_full <- weather[weight_df$id]
  # weather_full[is.na(weather_full)] <- 0
  # 
  # #results for each day using idw
  # candidates <- (as.matrix(weather_full) %*% as.matrix(weight_df['weight']))/sum(weight_df$weight)
  # 
  # #select days with missing data in target data and replace it with IDW data
  # weather[which(is.na(weather[,target])), target] <- candidates[which(is.na(weather[,target]))]
  # 
  # return(weather[,target])
  
}


patch_mice <- function(weather, target, meta_data, rain_data = T, 
                       prcp_threshold = 1, max.iter = 5, n.impute = 5, parallel = F){
  
  
  #columns to be ignored in the following
  ignore <- c('Year', 'Month', 'Day', 'Date')
  
  weather <- dplyr::select(weather, -all_of(ignore))
  

  if(rain_data){
    
    #make dummy dataframe of occurence
    dummy_df <- weather > prcp_threshold
    
    #find the most appropriate imputation method for target station
    methods <- mice::make.method(dummy_df)
    
    #set up predictor matrix, this function pre-selects most appropriate donors
    pred <- mice::quickpred(dummy_df)
    
    
    if(parallel){
      
      imp <- micemd::mice.par(dummy_df, predictorMatrix = pred, method = methods, 
                              m = n.impute, maxit = max.iter)
    } else{
      
      
      imp <- mice::mice(dummy_df,predictorMatrix = pred,
                        method = methods, maxit = max.iter, m = n.impute,
                        printFlag = F)
      
    }
    

    occ.imp <- mice::complete(data = imp, 'all')
    
    #function to get mode
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    #per imputation, bring imputed data of station to one dataframe
    occ.imp <- lapply(meta_data$id, function(x){
      #iteratr over the imputation versions
      extracted_station <- lapply(occ.imp, function(y){
        #extract station of interest
        y[x]
      })
      #bind different imputation versions of same station together
      extracted_station <- do.call(cbind, extracted_station)
      
      #return mode of different imputation versions of same station
      return(apply(extracted_station, MARGIN = 1, FUN = Mode))
      
    })
    
    #bind mode of different stations together
    occ.imp <- as.data.frame(do.call(cbind, occ.imp))

  }    
    
  #select appropriate predictors for target variable  
  pred <- mice::quickpred(weather)
  
  #find the most appropriate imputation method for station
  methods <- mice::make.method(weather)

  #find appropriate functions and run multiple imputations
  if(parallel){

    imp <- micemd::mice.par(weather, predictorMatrix = pred, method = methods,
                            m = n.impute, maxit = max.iter)
  } else{

    
    #impute rain amount 
    imp <- mice::mice(weather, predictorMatrix = pred, 
                      method = methods, printFlag = F, )
    
  }
  
  
  

  #load imputed data
  all.imp <- mice::complete(data = imp, 'all')
  
  
  #per imputation, bring imputed data of station to one dataframe
  all.imp <- lapply(meta_data$id, function(x){
    #iteratr over the imputation versions
    extracted_station <- lapply(all.imp, function(y){
      #extract station of interest
      y[x]
    })
    #bind different imputation versions of same station together
    extracted_station <- do.call(cbind, extracted_station)
    
    #return mode of different imputation versions of same station
    return(rowMeans(extracted_station))
    
  })
  
  #bind mean of different stations together
  all.imp <- as.data.frame(do.call(cbind, all.imp))
  
  if(rain_data){
    #set rain to zero, in case occ_imp sais no rain occured
    all.imp <- all.imp * occ.imp
    
    #furthermore: make sure there are no negative rainfall amounts
    all.imp[all.imp < prcp_threshold] <- 0
  }
  
  #adjust column names
  colnames(all.imp) <- meta_data$id
  
  return(all.imp)

}


patch_forest <- function(weather, target, meta_data, rain_data = F, prcp_threshold = 1, 
                         n_donors = 5, donor_criterion = 'closest', max.iter = 5){
  
  #columns to be ignored in the following
  ignore <- c('Year', 'Month', 'Day', 'Date')
  
  #drop columns which should not be imputed
  weather <- dplyr::select(weather, -ignore)
  
  if(donor_criterion == 'closest'){
    
    #extract lon and lat of target station
    target_lon <- meta_data[meta_data$id == target, 'longitude']
    target_lat <- meta_data[meta_data$id == target, 'latitude']
    
    #calculate distances between stations, choose the n closesest ones
    meta_data$distance <- round(sp::spDistsN1(as.matrix(meta_data[, c("longitude", "latitude")]),
                                              c(target_lon, target_lat), longlat = TRUE), 2)
    
    #sort to by increasing distance
    meta_data <- meta_data[order(meta_data$distance),]
    
  } else if(donor_criterion == 'correlation'){
    
    #calculate correlations, take only the ones regarding the target station
    weather.cor <- cor(weather, use = 'pairwise.complete.obs' )[,target]
    
    #create data frame of correlations, use names as id
    weather.cor <- data.frame(cor = weather.cor, id = names(weather.cor))
    
    #match with meta data
    meta_data <- merge.data.frame(meta_data, weather.cor, by = 'id')
    
    #sort metadata by decreasing correlations
    meta_data <-  meta_data[order(meta_data$cor, decreasing = T),]
  }
    
  #take donors from ordered meta_data 
  
  if(n_donors >= ncol(weather)){
    donors <- meta_data$id[2:nrow(meta_data)]
  } else {
    #case if we have enough donors
    donors <- meta_data$id[2:(n_donors + 1)]
  }
  
  #take data of target and donors
  weather <- dplyr::select(weather, all_of(c(target, donors)))

  
  #impute variable of interest
  weather.imp <- missForest::missForest(weather, maxiter = max.iter)
  
  #extract target column
  weather.imp <- weather.imp$ximp[,target]
  
  
  if(rain_data){
    
    #make dummy dataframe of occurence
    dummy_df <- as.data.frame(weather > prcp_threshold)
    
    #change columns to factors
    index <- 1:ncol(dummy_df)
    dummy_df[ , index] <- lapply(dummy_df[,index], as.factor)
    
    #impute occurence
    occ.imp <- missForest::missForest(dummy_df, maxiter = max.iter)
    occ.imp <-  occ.imp$ximp[,target]
    
    #correct imputed rain amounts by occurence
    weather.imp[occ.imp == FALSE] <- 0
    
    #prevent values below zero, set values below threshold to zero
    weather.imp[weather.imp < prcp_threshold] <- 0
  }
  
  return(weather.imp)
}



patch_amelia <- function(weather, target, meta_data, n.impute = 5, rain_data = F, prcp_threshold = 1,
                         parallel = NA){
  
  #bring date to date-format
  weather$Date <- as.Date(weather$Date)
  

  
  #only transform data if it is rain data
  if(rain_data){
    #bring in long format
    weather.long <- reshape2::melt(weather, id.vars = c('Date', 'Year', 'Month', 'Day'))
    
    #transform by log(x+1)
    weather.long$value <- log(weather.long$value + 1)
    
    #bring back to wide format
    weather <- reshape2::dcast(weather.long, formula = Date + Year + Month + Day ~ variable, value.var = 'value' )
  }

  #run amelia patching on weather data
  weather.imp <- Amelia::amelia(weather, ts = 'Date', m = n.impute,
                 idvars = c('Year', 'Month', 'Day'),parallel = parallel)
  
  #extract the imputed data from the object
  weather.imp <- weather.imp$imputations
  
  if(rain_data){
    #reverse transformation
    weather.imp <- lapply(weather.imp, function(x) exp(x[,meta_data$id])-1)
  }

  
  #bind imputation round by column for each station
  weather.sum <- lapply(meta_data$id, function(x){
    
    #take data of a certain weather station
    extracted_station <- lapply(weather.imp, function(y) y[,x])
    
    #bind it by columns for the different imputation rounds
    extracted_station <-  as.data.frame(do.call(cbind, extracted_station))
    
    #calculate row mean
    return(rowMeans(extracted_station))
    
  })
  
  #bind summarised data of the stations
  weather.sum <- as.data.frame(do.call(cbind, weather.sum))
  
  

  #in case of rain data, impute the occurence of rain and then correct the rain amount with occurence data
  if(rain_data){

    #drop unwatned columns
    ignore <- c('Date', 'Year', 'Month', 'Day')
    
    #create dummy dataframe with occurence
    dummy_df <- as.data.frame(dplyr::select(weather, -ignore) > prcp_threshold)
    
    #change columns to factors
    index <- 1:ncol(dummy_df)
    dummy_df[ , index] <- lapply(dummy_df[,index], as.factor)
    
    #add date as time series indicator
    dummy_df$Date <- weather$Date 
    
    #impute occurence
    occ.imp <- Amelia::amelia(dummy_df, m = n.impute, ts = 'Date', 
                              noms = meta_data$id, parallel =  parallel)
    
    #function to get mode
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    
    #bind imputation round by column for each station
    occ.sum <- lapply(meta_data$id, function(x){
      
      #take data of a certain weather station
      extracted_station <- lapply(occ.imp$imputations, function(y) y[,x])
      
      #bind it by columns for the different imputation rounds
      extracted_station <-  as.data.frame(do.call(cbind, extracted_station))
      #this step changes the logical to numerical with 1 and 2, because do.call binds to matrix, I guess...
      #1 == no rain, 2 == rain
      
      #calculate row mode of occurence
      return(apply(as.data.frame(extracted_station), MARGIN = 1, FUN = Mode))
      
    })
    
    #bind summarised data of the stations
    occ.sum <- as.data.frame(do.call(cbind, occ.sum))
    
    #change levels of occurence to a different code: 1 is rain occurence, 0 is no occurence
    occ.sum <- occ.sum - 1
    
    #--> multiply the data frames with each other, in cases there is no occurence the value is automatically set to zero
    weather.sum <- weather.sum * occ.sum
    
    #set prcp amount to zero in cases of amounts lower than threshold
    weather.sum[weather.sum < prcp_threshold] <- 0
    
  }
  
  #adjust column names
  colnames(weather.sum) <- meta_data$id
  
  #return imputed object
  return(weather.sum)
}





patch_climatol <- function(weather, target, meta_data){
  
  if( !('elevation' %in% colnames(meta_data))){
    #in case the elevation data was not provided in meta data, download it using elevatr package
    library(rgdal)
    elev <- elevatr::get_elev_point(location = meta_data[,c('longitude', 'latitude')], prj = "EPSG:4326")
    
    meta_data$elevation <- elev$elevation
    
  }
  
  if(sum(is.na(meta_data$elevation))>0){
    stop('Elevation in meta data contains at least one NA')
  }
  
  #bring meta data in format climatol requires
  meta_data <- meta_data[,c('longitude', 'latitude', 'elevation', 'id', 'name')]
  
  #min year
  min_year <- min(weather$Year)
  max_year <- max(weather$Year)
  
  #save object in workspace
  write.table(meta_data, paste0('var_',min_year,'-', max_year,'.est'), row.names=FALSE, col.names=FALSE)
  

  #make sure weather data columns are of the same order as in meta_data, also drops not needed columns
  weather <- weather[, meta_data$id]
  
  #write table of observations
  write(as.matrix(weather), file = paste0('var_',min_year,'-',max_year,'.dat'))
  
  #run patching
  climatol::homogen('var', as.numeric(min_year), as.numeric(max_year), dz.max = 100, snht1 = 0, gp = 0,verb = F)
  
  
  ####temp
  
  #load patched data
  #load(paste0('var_',min_year,'-',max_year,'.rda'))
  
  ####temp
  
  #adjust colnames
  dah <- as.data.frame(dah)
  colnames(dah) <- meta_data$id
  
  #remove files created by climatol
  pattern <- paste0('var_',min_year,'-',max_year)
  fnames <- list.files(pattern = pattern)
  unlink(x = fnames)
  
  #returns patched data of every station
  return(dah)
  
}

#work work work work work work work
#work on this!
#climatol already fixes all the stations, no need to run it for each variable independently
#make sure this is reflected in the implementation
#



#define function which can patch several stations and allows different kind of 
#patching method


patch_several_stations <- function(weather, target, meta_data, 
                                   method = 'mean_closest_stations'){
  
  if(method == 'mean_closest_stations'){
    patched_stations <- lapply(target, function(x){
      patch_mean(weather = weather, target = x, meta_data = meta_data, n_donors = 5)
    } )
    
    
    #bind list of data frames to one by column
    weather_patched <- do.call(cbind, patched_stations)
    #add Date
    weather_patched <- cbind.data.frame(weather$Date, weather_patched)
    #adjust colnames
    colnames(weather_patched) <- c('Date', target)
    
    
  } else if(method == 'ordinary_normal_ratio'){
    
    patched_stations <- lapply(target, function(x){
      patch_normal_ratio(weather = weather, target_station = x, 
                                  meta_data = meta_data, n_donors = 5, 
                         weight = 'ordinary')
    })
    
    #bind list of data frames to one by column
    weather_patched <- do.call(cbind, patched_stations)
    #add Date
    weather_patched <- cbind.data.frame(weather$Date, weather_patched)
    #adjust colnames
    colnames(weather_patched) <- c('Date', target)
    
  } else if(method == 'correlation_normal_ratio'){
    
    patched_stations <- lapply(target, function(x){
      patch_normal_ratio(weather = weather, target_station = x, 
                         meta_data = meta_data, n_donors = 5, 
                         weight = 'correlation')
    })
    
    #bind list of data frames to one by column
    weather_patched <- do.call(cbind, patched_stations)
    #add Date
    weather_patched <- cbind.data.frame(weather$Date, weather_patched)
    #adjust colnames
    colnames(weather_patched) <- c('Date', target)
  }
  
  return(weather_patched)
  
}




#this function can patch weather stations with any kind of patching function which is 
#defined by a loaded package or by a custom function in the environment
#REQUIREMENTS: - weather needs to be a data frame with a Date column named "Date" and in the following columns 
#                the observations per station of the SAME variable
#              - meta data needs to be defined: meta data needs to have column named 'id' which
#                are the same names as the column names in the weather object
#                needs to have columns called 'longitude' and 'latitude' in decimal format
#              - method needs to be the same name as the function call
#              - every additional argument needed for the patching function needs to go
#                into 'additional_arguments' list, each object needs to be named the same way
#                as in the function reffered in method


patch_flexible_several_stations <- function(weather, target, meta_data, 
                                   method = 'patch_mean', additional_input = list(n_donors = 5),
                                   method_patches_everything = F){
  

  
  
  #if the method automatically patches the whole dataframe, then there is no need to call it several times
  #this is true for climatol, amelia and mice
  #in these cases set target to the first target (because it shouldn't matter and return data for every station)
  if(method_patches_everything){
    
    #standard arguments for weather patching function call
    inputs <- list(weather = weather, target = target[1], meta_data = meta_data)
    
    #prepare additional argents that need to be added to the function call
    #only add additional arguments if there are any
    if(length(additional_input) == 1){
      
      #check if additional input is na, in that case do not add it to inputs
      if(!is.na(additional_input)){
        inputs <- c(inputs, additional_input)
      }
    } else{
      #case that there are more than one arguments in additional inputs, then add it no matter what
      inputs <- c(inputs, additional_input)
    }
    
    
    #call function with arguments
    weather_patched <- do.call(method, inputs)
    
    #only take weather stations specified in 'target' so that it is also compatible with function calls where 
    #the user is only interested in some of the stations
    weather_patched <- as.data.frame(weather_patched)[,target]
    
    # #add Date
    # weather_patched <- cbind.data.frame(weather$Date, weather_patched)
    # 
    # #adjust colnames
    # colnames(weather_patched) <- c('Date', target)
    

 
  } else {
    #case that the method only returns the fixed data of the target weather station
    
    #iterate over target stations
    patched_stations <- lapply(target, function(x){
      
      #standard arguments for weather patching function call
      inputs <- list(weather = weather, target = x, meta_data = meta_data)
      
      #prepare additional argents that need to be added to the function call
      #only add additional arguments if there are any
      if(length(additional_input) == 1){
        
        #check if additional input is na, in that case do not add it to inputs
        if(!is.na(additional_input)){
          inputs <- c(inputs, additional_input)
        }
      } else{
        #case that there are more than one arguments in additional inputs, then add it no matter what
        inputs <- c(inputs, additional_input)
      }
      
      
      # ###TEMP####
      # 
      # x<- target[10]
      # 
      # test <- lapply(target, function(x){
      #   
      #   #standard arguments for weather patching function call
      #   inputs <- list(weather = weather, target = x, meta_data = meta_data)
      #   
      #   #prepare additional argents that need to be added to the function call
      #   #only add additional arguments if there are any
      #   if(length(additional_input) == 1){
      #     
      #     #check if additional input is na, in that case do not add it to inputs
      #     if(!is.na(additional_input)){
      #       inputs <- c(inputs, additional_input)
      #     }
      #   } else{
      #     #case that there are more than one arguments in additional inputs, then add it no matter what
      #     inputs <- c(inputs, additional_input)
      #   }
      # 
      # test <- do.call(method, inputs)
      # return(sum(abs(test) > 50))
      # })
      # 
      # unlist(test)
      # 
      # ###TEMP####
      
      #temporary
      print(inputs$target)
      
      #call function on the inputs, IMPORTANT method must be the same name as the function in the environment
      return(do.call(method, inputs))
    })
    
    #bind list of data frames to one by column
    weather_patched <- do.call(cbind, patched_stations)
  }
  

  #add Date
  weather_patched <- cbind.data.frame(weather$Date, weather_patched)
  
  #adjust colnames
  colnames(weather_patched) <- c('Date', target)

  return(weather_patched)
  
}


#function input:  - weather data, organized as matrix with stations as columns, one matrix per variable
#                 - names of the targeted weather stationd
#                 - name of patching methods (need to be specified either in loaded libraries or in custom functions in the workspace, also need to have certain format)
#                 - percent missingness inserted in the weather data for evaluation
#                 - additional arguments needed for the patch function call, needs to be organized in list of lists, each element needs o have same name as in function call, if no further args needed use NA
#                 - indicate if the patch function patches the whole matrix or if it is applied only on certain columns (this influences if the function is called once or each target individually)
#                 - either it is a user-defined evaluation period for EVERY station, or it is defined by function for each station individually
#                 - decide if either only newly patched data is returned, if the whole evaluation period is returned or if the whole patched weather data is returned

#function output: - dataframe in long format with: Date, Year, Month, Day, station, and then original data and then per patch method a column of patched data

######
#EVALUATION FUNCTION
######


#function to evaluate ONE station of the dataset
get_eval_one_station <- function(weather, meta_data, target, patch_methods, p_missing, 
                                 additional_args, 
                                 method_patches_everything = F,
                                 period = NA, return_data = 'only_new_imputed'){
  
  #method patches everything needs to be of the same length as method
  
  
  #check if weather data contains columns of Date, Year, Month, Day
  if (!("Year" %in% colnames(weather) & "Month" %in% 
        colnames(weather) & "Day" %in% colnames(weather) & "Date" %in% colnames(weather))) 
    stop("Required input column 'Year', 'Month', 'Day' and/or 'Date' is missing.")
  

  ######
  #check period data
  ######
  
  #in case no period provided, in which holes should be punched
  if(length(period) == 1){
    if(is.na(period)){
      
      #apply the search of NA free period to all target stations
      #as a result have a list of the periods, in the order of the stations
      period_list <- lapply(target, function(x){
        
        #check if weather station has missing data
        if(sum(is.na(weather[x])) == 0){
          
          #in that case the period covers the whole dataframe
          return(c(1, nrow(weather)))
        } else {
          
          #look for the longest period without na
          longest_coverage <- na.contiguous(weather[,x])
          
          #get the row number of start and end of longest continous period
          period <- tsp(longest_coverage)
          
          return(period)
        }
      })
      
      
      
    } else if(is.list(period)){
      #case that there is a listof length = 1 and 
      period_list <- period[[1]]
    } else{
      error('Period in wrong format')
    }
  } else{
    #check if provided period is of right format:
    #can be supplied as list, but then it should have the same amount of elements as in target
    if(is.list(period)){
      if(length(period) != length(target)){
        stop('If period is supplied in form of list, then it should have the same length as target')
        
        #check the first element of period, if stored as list, should be of length two and should be numeric
      } else if(length(period[[1]]) != 2 | is.numeric(period[[1]]) == F){
        stop('Elements of period, when suppplied as a list, should be of length = 2 and should be numeric, in form of c(rownumber start, rownumber stop).')
      
        #case that everything is in right format
      } else if(length(period) == length(target) & length(period[[1]] == 2)){
          
        #assign period to period list
        period_list <- period
        }
        
        
      #if period supplied not in form of list, then it should be of length == 2 and numeric, as it is then applied on every station
    } else if(is.numeric(period)){
      if(length(period) != 2){
        stop('If period supplied in numeric form, then it needs to be in length of 2. Then it is applied on every target station')
      } else if(length(period) == 2){
        
        #if supplied in numeric form, bring it also to list form to be consistent
        period_list <- rep(list(period), length(target))
      }
      #if neither in form of list or in form of numeric, then invalid form 
    } else{
      stop('Period supplied in wrong format. Needs to be either in form of list (with same length as target, each element numeric of length two) or numeric (then length == 2, applied on every station.')
    }

  }
  

  
  #####
  #punch wholes in the different parts of the stations
  #####
  
  #period-list should be in the same order as target
  #--> add name to the period list as third element
  period_list <-  mapply(function(x,y){c(x,y)}, period_list, target, SIMPLIFY = F)
  
  # #prepare data, combine target with target name in a list
  # prep_data <- mapply(function(x,y){
  #   list(station = x, period = y)
  # }, target, period_list, SIMPLIFY = T)
  
  
  mod_weather <- lapply(period_list, function(x){
    
    #calculate how many Days should be removed from the station to achieve the desired missingness
    n_new_holes <- floor((as.numeric(x[2]) - as.numeric(x[1]) + 1) * p_missing)

    #sample from the rows of the target weather station, sampled rows get deleted
    row_holes <- sample(x = as.numeric(x[1]):as.numeric(x[2]), size = n_new_holes, replace = FALSE)
    
    prep_df <- data.frame(weather[,x[3]], new_na = FALSE)
    #add na at position of row holes
    
    prep_df[row_holes,1] <- NA
    prep_df[row_holes,2] <- TRUE
    
    colnames(prep_df) <- c(x[3], paste0(x[3], '_new_na'))
    
    return(prep_df)
  })
  
  # 
  # 
  # #for each data, punch random holes in the before identified period
  # prep_data <- apply(prep_data,MARGIN = 2, function(x){
  #   
  #   #calculate how many Days should be removed from the station to achieve the desired missingness
  #   n_new_holes <- floor((x[['period']][2] - x[['period']][1] + 1) * p_missing)
  #   
  #   #make a subset for the evaluation, which is the longest period of observation for the 
  #   #target station
  #   eval_sub <- weather[x[['period']][1]:x[['period']][2], x[['station']]]
  #   
  #   #sample from the rows of the target weather station, sampled rows get deleted
  #   row_holes <- sample(x = x[['period']][1]:x[['period']][2], size = n_new_holes, replace = FALSE)
  #   
  #   #punch holes in weather data
  #   weather[row_holes, x[['station']]] <- NA
  #   
  #   #make new data frame with new data of target station with holes, also add column which indicates where new NAs were created
  #   prepared_df <- data.frame(weather[, x[['station']]], new_na = FALSE)
  #   colnames(prepared_df) <- c(x[['station']], paste0(x[['station']], '_new_na'))
  #   prepared_df[row_holes,2] <- TRUE
  #   
  #   return(prepared_df)
  # 
  # })
  # 
  # #get rid of annoying names in list
  # prep_data <- unname(prep_data)
  
  #bind weather data
  mod_weather <- do.call(cbind, mod_weather)
  
  
  # #extract weather data (because there are also columns indicating if the NA is new or if it has been there before)
  # weather_eval <- prep_data[,target]
  # weather_eval <- cbind(weather[,c('Date', 'Year', 'Month', 'Day')], weather_eval)
  #weather eval should have the same format as weather, same columns
  #--> replace the columns of weather by targt
  # weather_eval <- weather
  
  #only take weather modified weather station data
  weather_eval <- cbind(weather[,c('Year', 'Month', 'Day', 'Date')], mod_weather[, target])
  
  #make sure that names are still correct
  names(weather_eval) <- c("Year", "Month", "Day", 'Date', target)
  
  #dataframe indicating if new hole created per weather station targeted
  holes_df <- dplyr::select(mod_weather, -all_of(target))
  
  #add date column
  holes_df <- cbind(Year = weather$Year, Month = weather$Month, Day = weather$Day,
                    Date = weather$Date, holes_df)

  
  

  #####
  #run patching methods
  #####
  
  #problem: if I want to run this function, then I need also a list of list with 
  #additional arguments
  
  #I make evaluation only for one station but have model call for all stations, that doesnt make sense!
  #waste of computing time
  

  #there is a problem in patch several flexible: prblem when combining the model outcomes of everything patched and itereatively patched
  
  #bind everything together in one list, per elemt have the items of additional arguments, method_patches_everything and method in one item
  prep_data <- mapply(function(x,y,z){
    list(method = x, additional_args = y, method_patches_everything = z)
  }, patch_methods, additional_args, method_patches_everything, SIMPLIFY = F)
  
  
  #run patching function on evaluation data
  patched <- lapply(prep_data, function(x){
    
    #give info which method is now used
    print(paste0('Start patching method: ', x[['method']]))
    
    #carryout patch function
    patched <- invisible(patch_flexible_several_stations(weather = weather_eval, 
                                                         target = target, 
                                                         meta_data = meta_data, 
                                                         method = x[['method']],
                                                         additional_input = x[['additional_args']], 
                                                         method_patches_everything = x[['method_patches_everything']]))
    
    #bring to long format
    patched <- reshape2::melt(patched, measure.vars = target, 
                              variable.name = 'station', value.name = x[['method']])
    
    #return only value column of long format patched
    return(patched)
  })
  
  patched <- unname(patched)
  
  #output is list with station and value per patch method
  #--> bind them by column
  patched <- do.call(cbind.data.frame, patched)
  
  #duplicated columns of date and station; at first adjust column names
  names(patched) <- make.unique(names(patched), sep = '_')
  
  #adjust names of patch_methods if duplicated
  if(sum(duplicated(patch_methods)) > 0){
    patch_methods <- make.unique(patch_methods, sep = '_')
    warning('At least two patching methods have same name, column names were made unique by appending _1 on the second duplicated patching name and so on')
  } 
  
  #only take Year, Month, Day, Date and patch_methods
  patched <- patched[, c('Date', 'station', patch_methods)]
  
  
  #add info of weather station because patched is already in long format
  holes_df <- reshape2::melt(holes_df, variable.name = 'station', 
                             id.vars = c('Year', 'Month', 'Day', 'Date'), value.name = 'new_na')
  #strip the _new_na from the variable var
  holes_df$station <- gsub(pattern = '_new_na', replacement = '', x = holes_df$station)
  
  
  #merge the two dataframes 
  patched <- dplyr::left_join(x = patched, y = holes_df, by = c('Date', 'station'))
  

  #add info on evaluation period
  period_list <- lapply(period_list, function(x){
    period_vec <- rep(F,nrow(weather_eval))
    period_vec[as.numeric(x[1]):as.numeric(x[2])] <- T
    
    return(period_vec)
  })

  #add info which part belongs to evaluation period
  patched$eval_period <- do.call(c, period_list)
  
  #add original weather to patched
  weather_long <- reshape2::melt(weather, id.vars = c('Year', 'Month', 'Day', 'Date'),
                                 value.name = 'original', variable.name = 'station')
  
  patched <- dplyr::left_join(x = patched, y = weather_long, 
                              by = c('Year', 'Month', 'Day', 'Date', 'station'))
  
  #now either return everything, everyhtin where eval_period is true or everything where new_na is true
  
  
  
  #three options: 1) return only the imputed data 
  #               2) return the whole evaluation period per station
  #               3) return the complete patched weather stations
  
  
  #option 1: return only newly imputed data
  if(return_data == 'only_new_imputed'){
    return(patched[patched$new_na, c('Date', 'Year', 'Month', 'Day', 'station', 'original', patch_methods)])
  
  #option 2: return the whole period, which was used to randomly insert NAs  
  } else if(return_data == 'evaluation_period' ){
    return(patched[patched$eval_period, c('Date', 'Year', 'Month', 'Day', 'station', 'original', patch_methods, 'new_na')])
  
  #option 3: return everything
  } else if(return_data == 'everything'){
    
    return(patched[, c('Date', 'Year', 'Month', 'Day', 'station', 'original', patch_methods, 'new_na', 'eval_period')])
  }

}



#####
#EVALUATION METRICS
#####


get_MAE <- function(predicted, observed, na.rm = FALSE){
  if (!na.rm) 
    if (!(length(which(is.na(predicted))) + length(which(is.na(observed)))) == 
        0) 
      stop("Datasets include NA values. This may indicate a serious prediction problem. To override this error, set na.rm=TRUE.")
  
  mae <- sum(abs(observed - predicted)) / length(which(!is.na(observed - predicted)))
  return(mae)
}

get_NSE <- function(predicted, observed){
  
  return(1-(sum((observed - predicted)^2) / (sum((observed - mean(observed, na.rm = T))^2))))
}


get_KS_test <- function(predicted, observed){
  res <- ks.test(observed, predicted)
  return(res$p.value)
}

get_S_index <- function(predicted, observed){
  pred_mean <- mean(predicted, na.rm = T)
  obs_mean <- mean(observed, na.rm = T)
  
  return((sum((predicted - pred_mean) * (observed - obs_mean)) / sqrt(sum((predicted - pred_mean)^2) * sum((observed - obs_mean)^2)))^2)
}

get_skill_score <- function(predicted, observed){
  
  mse_1 <- sum((predicted - observed)^2) / length(which(!is.na(observed - predicted)))
  mse_2 <- sum((mean(observed, na.rm = T) - observed)^2) / length(which(!is.na(observed - predicted)))
  
  return(1 - (mse_1 / mse_2))
}

get_coef_of_efficiency <- function(observed, predicted){
  return(1 - (sum((observed - predicted)^2) / sum((observed - mean(observed, na.rm = T))^2)))
}


#this score expresses: if the model makes a prediction on a state, how often do is the model right about it (assuming we have equal distribution in our labels)
get_hit_score <- function(observed, predicted){
  
  #transform precipitation data to occurence data
  observed_occ <-  observed > 0
  predicted_occ <- predicted > 0
  
  #get share of correctly identified wet days to all predicted days of precipitation
  no_wet_pred <- sum(predicted_occ)
  if(no_wet_pred == 0){
    stop("The model only predicted dry days and never a rainy day. Seems odd. Also this forces to devide by 0, so no hit fraction could be calculated")
  }
  no_correct_wet <- sum(predicted_occ == T & observed_occ == T)
  h1 <- no_correct_wet / no_wet_pred
  #if it predicts wet, how often is it really wet?
  
  #get share of correctly identified dry days of all predicted dry days
  no_dry_pred <- sum(!predicted_occ)
  if(no_dry_pred == 0){
    stop("The model only predicted rain and never a dry day. Seems odd. Also this forces to devide by 0, so no hit fraction could be calculated")
  }
  no_correct_dry <- sum(predicted_occ == F & observed_occ == F)
  h0 <- no_correct_dry / no_dry_pred
  #if it predicts dry, how often is it really dry?
  
  return((h0 + h1)/2)

}
#this gives us only info about the recall: how many relevant items were retrieved?

#mathews correlation coefficient
get_MCC <- function(observed, predicted){
  
  #transform precipitation data to occurence data
  observed_occ <-  observed > 0
  predicted_occ <- predicted > 0
  
  #get confusuin matrix elements
  TP <- sum(observed_occ == T & predicted_occ == T) / length(observed_occ)
  TN <- sum(observed_occ == F & predicted_occ == F) / length(observed_occ)
  FN <- sum(observed_occ == T & predicted_occ == T) / length(observed_occ)
  FP <- sum(observed_occ == F & predicted_occ == T) / length(observed_occ)
  
  #get number of total predicted/observed wet days / dry days
  share_wet_obs <- sum(observed_occ == T) / length(observed_occ)
  share_wet_pred <- sum(predicted_occ == T) / length(observed_occ)
  share_dry_obs <- sum(observed_occ == F) / length(observed_occ)
  share_dry_pred <- sum(predicted_occ == F) / length(observed_occ)
  
  #calculate mathews correlation coefficient
  return(((TP * TN) - (FP * FN)) / sqrt(share_dry_obs * share_wet_obs * share_dry_pred * share_wet_pred))
}

get_hanssen_kuipers <- function(observed, predicted){
  
  #transform precipitation data to occurence data
  observed_occ <-  observed > 0
  predicted_occ <- predicted > 0
  
  #get confusuin matrix elements
  A <- sum(observed_occ == T & predicted_occ == T) / length(observed_occ)
  D <- sum(observed_occ == F & predicted_occ == F) / length(observed_occ)
  C <- sum(observed_occ == T & predicted_occ == F) / length(observed_occ)
  B <- sum(observed_occ == F & predicted_occ == T) / length(observed_occ)
  
  return( ((A*D)-(B*C))/((A+B)*(C+D)) )
}
#value of 0 indicates no skill in prediction
#value of 1 is the best

get_d_index <- function(predicted, observed){
  
  mean_obs <- mean(observed)
  n <- length(predicted)
  
  if(sum(abs(predicted - observed)) <= 2*sum(abs(observed - mean_obs))){
    
    return(1 - (sum(abs(predicted - observed)) / (2*sum(abs(observed - mean_obs)))))
    
  } else {
    return(((2*sum(abs(observed - mean_obs))) / (sum(abs(predicted - observed)))) - 1)
  }
}


#format of eval data:
#                     -one column stating patch_name: called patch_method
#                     -one column stating patch value: called value
#                     -one column for original data: called original


get_eval_metrics <- function(eval_df, eval_fun = c('get_MAE', 'RPIQ', 'RMSEP', 'cor'), 
         calc_summary_score = T, patch_fun, bigger_better = c(F,T,F,T), weights = NA){
  
  
  #check if evaluation functions and bigger_better are of same size
  if(calc_summary_score)
    if(length(bigger_better) != length(eval_fun))
      stop('Length of evaluation functions and vector indicating if bigger score is 
         better need to be of same length, when calculating a summary score on all metrics')
  
  
  #chechk if patch fun contains duplicated names, if so adjust them to the format by appending .1 
  # if(sum(duplicated(patch_fun)) > 0){
  #   patch_fun <- make.unique(patch_fun, sep = '_')
  # }
  
  #make eval long
  # eval_long <- reshape2::melt(eval_df, measure.vars = patch_fun,  variable.name = 'patch_method' )
  
  eval_long = eval_df
  
  #split dataframe to list
  eval_list <- split(eval_long, f = list(eval_long$station, eval_long$patch_method))
  
  #calculate each metric for the combination of patching method and patched weather station
  eval_out <- lapply(eval_list, function(x){
    scores <- lapply(eval_fun, function(y){
      do.call(y,list(x$value, x$original))
    })
    #add info of how many datapoints were used for calculation of metric
    return(append(scores, nrow(x)))
    
  })
  
  #bring eval_out back into the desired 
  eval_metric <- data.table::rbindlist(lapply(eval_out, c))
  colnames(eval_metric) <- c(eval_fun, 'n')
  
  
  #get names of stations and patch method
  id.vars <- (strsplit(names(eval_list), split = '[.]'))
  
  #join them row-wise to data frame
  id.vars <- lapply(id.vars, function(x) as.data.frame(t(x)))
  id.vars <- bind_rows(id.vars)
  colnames(id.vars) <- c('station', 'patch_method')
  
  #only take columns of station and patch_method
  id.vars <- id.vars[,c('station', 'patch_method')]
  
  #join info of station & patch method withh metric outcomes
  eval_metric <- cbind(id.vars, eval_metric)
  
  #drop nans from eval_metric
  eval_metric <- na.omit(eval_metric)
  
  
  #get a summary score which harmonizes all the metric scores
  if(calc_summary_score == T){
    
    #if no weights provided, then everything is set to 1 (so equally weighted)
    if(length(weights) == 1){
      if(is.na(weights)){
        weights <- rep(1, length(eval_fun))
      }
    }
    
    intermed <- eval_metric[, eval_fun]
    
    #negative scores cause problems for the overall score calculation, so if there
    #is a negative value, then rescale this score for the total score calculation
    neg_value_present <- colSums(eval_metric[, eval_fun] < 0) > 0

    if(any(neg_value_present)){
      
      #rescale column with negative value
      target_col <- eval_fun[neg_value_present]
      
      mins <- apply(eval_metric[target_col], MARGIN = 2, min)
      
      #rescale, so that min is 0
      intermed[,target_col] <- eval_metric[target_col] + matrix(abs(mins), 
                                                                ncol = length(target_col), 
                                                                nrow = nrow(intermed), byrow = T)
      
    
    }
    
    #get maximum value per metric
    max_metric <- intermed[,eval_fun] %>%
      summarise_if(is.numeric, max)
    
    #bring weights to format of data frame
    weight_df <- rbind.data.frame((weights) / max_metric)
    
    #give the weights the same length as the eval data frame
    weight_df <-  weight_df[rep(1,nrow(eval_metric)),]
    
    intermed <- intermed[,eval_fun] * weight_df
    
    #inverse score for metrics where bigger is better
    intermed[, bigger_better] <- 1- intermed[,bigger_better]
    
    
    #calculate sum of rescaled metrics
    eval_metric$score <-   rowSums(intermed)
    
    #rescale the score: high score should indicate good performance
    max_score <- max(eval_metric$score)
    
    #rescale so that max scale gets a score of 0 (because it is the worst)
    eval_metric$score <- max_score - eval_metric$score
    
  }
  
  #get rid of any 'get_' in the colnames
  colnames(eval_metric) <- gsub(pattern = 'get_', replacement = '',  colnames(eval_metric))
  
  return(eval_metric)
  
}



