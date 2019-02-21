#' Reads in shapefile and fortifies it
#' @param file Shapefile location
#' @param region Name of the variable to fortify on
#' @export FortifyShapeFile
FortifyShapeFile <- function(file, region) {
  map <- maptools::readShapeSpatial(file)
  fortifiedMap <- broom::tidy(map, region = region)
  return(fortifiedMap)
}
