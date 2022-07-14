#' Impute gaps in daily weather data using multiple imputation of mice package
#' 
#' Takes data.frame of daily weather data of several neighboring weather stations
#' and imputes gaps with multiple imputation method of the mice-package.
#' 
#' For more details of the function please refer to \code{\link{mice::mice}}
#' 
#' @param weather data.frame with columns for each weather station and rows for
#' each daily observation. All columns need to contain observations of the same
#' variable. Missing observations need to be marked with NA
#' @param target character, column name in weather, on which the method should be applied
#' @param weather_info data.frame containing the name / id of the weather station (needs
#' to be the same as the column names in weather), Longitude and Latitude in
#' decimal format
#' @param rain_data logical, in case precipitation data should be patched the 
#' mice package allows to at first decide if precipitation occures and in a
#' second step to patch precipitation amount only in cases where the patching
#' algorithm expects precipitation to happen.
#' @param prcp_threshold numerical, setting the threshold until which
#' precipitation amount the algorithm treats observations are real precipitation
#' events. Values below are treated as zeros
#' @param max.iter maximum amount of iterations of the multiple imputation algorithm,
#' default is 5
#' @param n_impute number of multiple imputations, default is 5
#' @param parallel logical, if true the paralleled version of \code{\link{mice::mice}} 
#' called \code{\lin{micemd::mice}} will be used instead
#' @return same data.frame as weather but with all NAs imputed for all columns. There
#' can be still cases of NA, if for a certain observation none of the other stations
#' had valid observations
#' @examples #think of example here
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
patch_mice <- function(weather, target, weather_info, rain_data = T, 
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
    occ.imp <- lapply(weather_info$id, function(x){
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
  all.imp <- lapply(weather_info$id, function(x){
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
  colnames(all.imp) <- weather_info$id
  
  return(all.imp)
  
}