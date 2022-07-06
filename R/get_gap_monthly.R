#' helper function: detects climatological gaps in monthly weather data
#' 
#' The function takes weather data (usually all the observations for a certain month
#' throughout all years) and checks if there are suspicious large gaps towards the tails
#' of the distribution.
#' 
#' In case of temperature data both tails of the distribution are evaluated,
#' taking the median as the starting value. In case of precipitation only the
#' upper tail is evaluated.
#' 
#' This is a helper function for the higher-level \code{link{perform_gap_check}}
#' 
#' @param x numerical vector, usually either daily temperature or precipitation data
#' @param temp flag if it is temperature data, in case of precipitation set it as FALSE
#' @param gap_threshold testing threshold, if gap at the edge of the distribution
#' is larger than that, then all values towards the tail of the gap are flagged
#' @return logical vector of the size of x, TRUE indicates that the data was
#' flagged because of a gap in the ordered data larger than the gap_threshold
#' @examples get_gap_monthly(x = weather[weather$Month == 1, "Tmin"])
#' @seealso \code{link{perform_gap_check}}
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
get_gap_monthly <- function(x, temp = TRUE, gap_threshold = 10){
  
  gap_flag <- rep(FALSE, length(x))
  
  #step to take for the iterations
  step <- 1
  
  if(temp == TRUE){
    #detect position of median. make this the new start for the search
    start <- which_quantile(x,probs = 0.5)
    
    #increment for next iteration, in case of temperature go up and go down
    step <- c(1,-1)
    
  } else{
    #in case of precipitation, start at first non-zero value
    start <- min(which(x > 0))
  }
  
  for(inc in step){
    gap_found <- FALSE
    i <- start
    
    while(gap_found == FALSE){
      
      i <- i + inc
      
      #once the subsript gets out of the valid options, stop the algorithm
      if(i %in% c(0, length(x))){
        break()
      }
      
      if(diff(x)[i] >= gap_threshold){
        gap_found <- TRUE
      }
    } #end of while loop
    
    #in case a gap was found, adjust the flag vector
    if(gap_found == TRUE){
      #if inc is positive, set everything above as outlier
      if(inc == 1){
        gap_flag[(i+1):length(x)] <- TRUE
      } else{
        gap_flag[1:i] <- TRUE
      } 
    }
    
    
    
  }#end of for loop
  
  
  return(gap_flag)
}
