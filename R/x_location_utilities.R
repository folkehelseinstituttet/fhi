#' location utilities



#'
#' @param location_code a location code
#' 
#' @export get_location_name

get_location_name <- function(lc){
  return(fhidata::norway_locations_long_current[location_code == lc, location_name])
}
