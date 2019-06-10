#' Topographic least cost paths and distances
#'
#' Calculates topographic least cost distances and paths
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data.
#' @param costSurface A RasterLayer for the conductance (inverse of resistance) values for each cell.
#' @param pts A SpatialPoints object or two-column matrix with xy coordinates for the geographic points from which to calculate pairwise distances and paths.
#' @param directions numberic (default = 8). The number of directions for movement between cells, either 4 or 8.
#' @param paths logical. Default is FALSE, in which case only topographic distances are calculated.  If TRUE, topographic paths are also identified.
#' @return Matrix of topographic distances (if paths = FALSE), or a list containing a matrix of topographic distances and the topographic paths as an object of class SpatialLines (if paths = TRUE).
#' @details
#' The values of the raster for costSurface should be conductance values rather than resistance values.  These can be calculating by taking the inverse of resistance values.
#' @examples
#' xy <- matrix(ncol = 2, byrow = TRUE,
#'    c(-119.5566, 37.72474,
#'    -119.4718, 37.76078))
#' topoLCP(Yosemite$DEM, Yosemite$SDM, xy, paths = TRUE)
#' @import gdistance
#' @import sp
#' @export
topoLCP <- function(DEM, costSurface, pts, directions = 8, paths = FALSE){
  pts <- SpatialPoints(pts)
  t.dist <- topoSurface(DEM, directions = 8)
  lcp.dist <- transition(costSurface, transitionFunction = mean, directions = 8)
  tlcp <- t.dist * lcp.dist
  topoDistances <- as.matrix(costDistance(tlcp, pts))
  if (paths){
    paths <- topoPaths(tlcp, pts)
    return(list(topoDistances, paths))
  } else {
    return(topoDistances)
  }
}
