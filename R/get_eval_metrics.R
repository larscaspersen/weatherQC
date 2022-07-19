#' Calculate evaluation metrics for weather imputation analysis
#' 
#' This function takes the outcomes obtained by \code{\link{get_eval_one_station}}
#' and calculates several evaluation metrics based on the outcomes.
#' 
#' This is a flexible high-level function similar to 
#' \code{\link{patch_flexible_several_stations}}. It is compatible with
#' evaluation metrics supplied with this packaged, but also works with other
#' packages evaluation metrics or user-defined functions. The only requirements
#' are the arguments predicted and observed. If the function has differently
#' named arguments a wrapper function might be necessary.
#' 
#' The function allows also the calculation of a summary score, of the different
#' evaluation scores. This idea was taken from \insertCite{teegavarapu_missing_2014;textual}{weatherQC}. In case of negative
#' values the score re-scales the values, so that 0 is the lowest observed 
#' score, otherwise the score can't handle negative values. It is possible
#' to assign weights to the individual metrics. If no weights are specified, then
#' they are equally weighted.
#' 
#' @param eval_df data.frame with the columns c(station, patch_method, value and 
#' original)
#' @param eval_fun character vector of the evaluation metric functions which should
#' be applied. Needs to be exactly the functions name without the parantheses ()
#' @param calc_summary_score boolean, decides if summary score is calculated
#' @param bigger_better boolean vector, needs to be the same length as 
#' \code{eval_fun}. Value of TRUE indicates that higher values mean better performance,
#' FALSE for scores where lower scores indicate better performance. Order needs to
#' be the same as in \code{eval_fun}
#' @param weights numeric vector of same length as \code{eval_fun} and \code{bigger_better}.
#' If supplied, should contain positive values
#' @return data.frame with station_name, name of evaluation metric and value for
#' evaluation metric
#' @examples 
#' #patch weather stations
#' patched <- get_eval_one_station(weather = weather_Tmin,
#' weather_info = rbind(target_info, neighbour_info),
#' target = 'cimis_2', 
#' patch_methods = c('patch_idw','patch_normal_ratio'), 
#' method_patches_everything = c(TRUE, FALSE))
#' 
#' #bring result to long format
#' patched_long <- reshape2::melt(patched, measure.vars = c('patch_idw','patch_normal_ratio'),
#'                variable.name = 'patch_method')
#' 
#' #calculate evaluation scores
#' get_eval_metrics(eval_df = patched_long)
#' 
#' @references
#' \insertAllCited{}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
get_eval_metrics <- function(eval_df, 
                             eval_fun = c('calc_MAE', 'chillR::RPIQ', 
                                          'chillR::RMSEP', 'stats::cor'), 
                             calc_summary_score = T, 
                             bigger_better = c(F,T,F,T), 
                             weights = NULL){
  
  
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
  
  #helperfunction to parse :: in eval_fun
  getfun<-function(x) {
    if(length(grep("::", x))>0) {
      parts<-strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }
  #helper function taken from
  #https://stackoverflow.com/questions/38983179/do-call-a-function-in-r-without-loading-the-package
  
  
  #calculate each metric for the combination of patching method and patched weather station
  eval_out <- lapply(eval_list, function(x){
    scores <- lapply(eval_fun, function(y){
      do.call(getfun(y),list(x$value, x$original))
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
  id.vars <- dplyr::bind_rows(id.vars)
  colnames(id.vars) <- c('station', 'patch_method')
  
  #only take columns of station and patch_method
  id.vars <- id.vars[,c('station', 'patch_method')]
  
  #join info of station & patch method withh metric outcomes
  eval_metric <- cbind(id.vars, eval_metric)
  
  #drop nans from eval_metric
  eval_metric <- stats::na.omit(eval_metric)
  
  
  #get a summary score which harmonizes all the metric scores
  if(calc_summary_score == T){
    
    #if no weights provided, then everything is set to 1 (so equally weighted)
    if(is.null(weights)){
      weights <- rep(1, length(eval_fun))
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
      dplyr::select(is.numeric) %>%
      dplyr::summarise_all(max)
    
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
  
  #get rid of package extension like stat:: from column names
  colnames(eval_metric) <- gsub(pattern = '.*::', replacement = '',  colnames(eval_metric))
  
  return(eval_metric)
  
}
