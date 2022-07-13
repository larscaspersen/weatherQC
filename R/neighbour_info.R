#' Metadata of neighbouring weather stations
#'
#' data.frames containing a unique identifier for the weather station, coordinates
#' in decimal format and name of the weather startion.
#'
#' @format data.frame with 12 row and 6 variables:
#' \describe{
#'   \item{id}{unique identifies of weather station}
#'   \item{Name}{name of weather station}
#'   \item{Longitude}{longitude of weather station decimal format}
#'   \item{Latitude}{latitude of weather station in decimal format}
#' }
#' @usage data(neighbour_info)
#' @docType data
#' @source \url{https://cimis.water.ca.gov/} and \url{http://ipm.ucanr.edu/}
"neighbour_info"