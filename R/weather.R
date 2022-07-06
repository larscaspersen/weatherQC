#' Daily weather observations Five Points (California)
#'
#' A data set containing the daily observations of precipitation, minimum and 
#' maximum temperature recorded at the weather station Five Points (California). 
#' Data was obtained using chillR::handle_cimis() function.
#'
#' @format A data frame with 11688 rows and 14 variables:
#' \describe{
#'   \item{Weather Station: Name of station}
#'   \item{Year}
#'   \item{Month}
#'   \item{Day}
#'   \item{Tmax: Maximum temperature (degree C)}
#'   \item{Tmin: Minimum temperature (degree C)}
#'   \item{Precip: Daily Precipitation (mm)}
#'   \item{QC_Tmax: Quality flag of maximum temperature}
#'   \item{QC_Tmin Quality flag of minimum temperature}
#'   \item{QC_Precip: Quality flag of precipitation}
#'   \item{Tmean: Average daily temperature (degree C)}
#'   \item{QC_Tmean: Quality flag of average daily temperature}
#'   \item{Date}
#'   \item{doy: Day of the year}
#' }
#' @source \url{https://cimis.water.ca.gov/}
"weather"