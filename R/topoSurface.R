#' Topographic distance surface
#'
#' Generates a TransitionLayer object for topographic distance from a RasterLayer
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data.
#' @param conductance logical (default = TRUE). If FALSE, resistance values are returned.  If TRUE, conductance values (1/resistance) are returned.
#' @param directions numeric (default = 8). Directions of allowable movement between raster cells (4 or 8).
#' @param zweight numeric (default = 1). The weight to be applied to the elevation (z) distances relative to the horizontal (xy) distances.
#' @return TransitionLayer
#' @details
#' This function generates a TransitionLayer from a DEM, which is used by the topoDist and topoLCP functions.  It does not need to be called separately from the topoDist and topoLCP functions.
#' @examples
#' YosTL <- topoSurface(Yosemite$DEM)
#' @import gdistance
#' @importFrom raster adjacent ncell
#' @export
topoSurface <- function(DEM, conductance = TRUE, directions = 8, zweight = 1){
  h.dist <- gdistance::transition(DEM, transitionFunction = function(x){1}, directions = directions, symm = TRUE)
  h.dist <- gdistance::geoCorrection(h.dist, scl = FALSE)
  adj <- raster::adjacent(DEM, cells = 1:ncell(DEM), pairs = TRUE, directions = directions)
  h.dist[adj] <- 1/h.dist[adj]
  elevDiff <- function(x){zweight * abs(x[2] - x[1])}
  v.dist <- gdistance::transition(DEM, elevDiff, 8, symm = TRUE)
  t.dist <- v.dist
  if (conductance){
    t.dist[adj] <- 1/sqrt((h.dist[adj]^2)+(v.dist[adj]^2))
  } else {
    t.dist[adj] <- sqrt((h.dist[adj]^2)+(v.dist[adj]^2))
  }
  return(t.dist)
}
