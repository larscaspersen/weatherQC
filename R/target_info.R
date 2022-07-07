#' Metadata of target weather station
#'
#' data.frames containing a unique identifier for the weather station, coordinates
#' in decimal format and name of the weather startion.
#'
#' @format data.frame with 1 row and 6 variables:
#' \describe{
#'   \item{id}{unique identifies of weather station}
#'   \item{Name}{name of weather station}
#'   \item{Longitude}{longitude of weather station decimal format}
#'   \item{Latitude}{latitude of weather station in decimal format}
#'   \item{Start_date}{first available day of weather observation of this station}
#'   \item{End_date}{last available day of weather observation of this station}
#' }
#' @source \url{https://cimis.water.ca.gov/}
"target_info"