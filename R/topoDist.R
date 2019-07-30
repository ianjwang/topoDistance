#' Topographic distances and paths
#'
#' Calculates shortest topographic distances and paths
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data.
#' @param pts A SpatialPoints object or two-column matrix with xy coordinates for the geographic points from which to calculate pairwise distances and paths.
#' @param directions numeric (default = 8). The number of directions for movement between cells, either 4 or 8.
#' @param paths logical. Default is FALSE, in which case only topographic distances are calculated.  If TRUE, topographic paths are also identified.
#' @param zweight numeric (default = 1). The weight to be applied to the elevation (z) distances relative to the horizontal (xy) distances.
#' @return Matrix of topographic distances (if paths = FALSE), or a list containing a matrix of topographic distances and the topographic paths as an object of class SpatialLines (if paths = TRUE).
#' @details
#' If paths = FALSE, the function will return a matrix of pairwise topographic distances between the specified points.
#' If paths = TRUE, the function will return a list with two items: (1) the matrix of pairwise topographic distances,
#' and (2) a SpatialLines object containing the topographic paths.
#' @examples
#' xy <- matrix(ncol = 2, byrow = TRUE,
#'    c(-119.5566, 37.72474,
#'    -119.4718, 37.76078))
#' topoDist(Yosemite$DEM, xy, paths = TRUE)
#' @importFrom gdistance costDistance
#' @importFrom sp SpatialPoints
#' @export
topoDist <- function(DEM, pts, directions = 8, paths = FALSE, zweight = 1){
  pts <- sp::SpatialPoints(pts)
  t.dist <- topoSurface(DEM, directions = directions, zweight = zweight)
  topoDistances <- as.matrix(gdistance::costDistance(t.dist, pts))
  if (paths){
    paths <- topoPaths(t.dist, pts)
    return(list(topoDistances, paths))
  } else {
    return(topoDistances)
  }
}
