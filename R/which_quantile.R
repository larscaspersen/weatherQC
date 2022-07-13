#' returns closest value to percentile
#' 
#' The function returns the value of a numerical vector which is closest to the
#' value of the requested percentile. It is the equivalent of \code{\link{which.max}}
#' or \code{\link{which.min}}.
#' 
#' The function was not written by me but I found it on stackexchange, but I can't find
#' the respective link anymore. 
#' 
#' @param x numeric vector
#' @param probs percentile, which is searched for
#' @param na.rm flag, to remove NA values from x
#' @return value in x which is closest to the requested percentile
#' @examples 
#' which.quantile(x = c(1:4), probs = 0.5)
#' @noRd
which_quantile <- function (x, probs, na.rm = FALSE){
  if (! na.rm & any (is.na (x)))
    return (rep (NA_integer_, length (probs)))
  
  o <- order (x)
  n <- sum (! is.na (x))
  o <- o [seq_len (n)]
  
  nppm <- n * probs - 0.5
  j <- floor(nppm)
  h <- ifelse((nppm == j) & ((j%%2L) == 0L), 0, 1)
  j <- j + h
  
  j [j == 0] <- 1
  o[j]
}