#' List of neighboring stations weather data
#'
#' List of data.frames with weather data of weather stations withing a 75 km of 
#' target weather station Five Points (California). The individual data.frames contain
#' daily weather observations similarly structured as in \code{\link{target_weather}}.
#' Data was downloaded using using \code{chillR::handle_cimis()} and
#' \code{chillR::handle_ucipm()} functions.
#'
#' @format A list of 12 data.frames with varying amount rows and 8 variables:
#' \describe{
#'   \item{Weather Station}{Name of station}
#'   \item{Year}{Year}
#'   \item{Month}{Month}
#'   \item{Day}{Day}
#'   \item{Tmax}{Maximum temperature (degree C)}
#'   \item{Tmin}{Minimum temperature (degree C)}
#'   \item{Precip}{Daily Precipitation (mm)}
#'   \item{Date}{Date}
#' }
#' @source \url{https://cimis.water.ca.gov/} and \url{http://ipm.ucanr.edu/}
"neighbour_weather"