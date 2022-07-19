#' Return the p-value of the Kolmogorov - Smirnov test
#' 
#' This function carries out a Kolmogorov - Smirnov test and returns the test's p-value.
#' 
#' This function is a simple wrapper function for \code{\link{ks.test}}.
#' A two-sided KS-test is carried out and the p-value is returned. The KS-tests
#' nullhypothesis is, that predicted and observed values were drawn from the same
#' continuous distribution. NA values are silently omitted by the KS-test.
#' 
#' @param predicted numeric vector containing predicted values
#' @param observed numeric vector containing observed values, should be of same
#' length as \code{predicted}
#' @return p-value of KS-test
#' @examples calc_KS_test(predicted = 1:10, observed = 2:11)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @export
calc_KS_test <- function(predicted, observed){
  res <- stats::ks.test(observed, predicted)
  return(res$p.value)
}