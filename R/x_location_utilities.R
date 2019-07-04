#' location utilities
#' @import fhidata


#'
#' @param location_code a location code
#' @export get_location_name
get_location_name <- function(location_code) {
  location_name <- NULL
  return(fhidata::norway_locations_long_current[location_code, on = "location_code", location_name])
}
