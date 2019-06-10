#' Identify shortest topographic paths
#'
#' @param t.dist A TransitionLayer object.
#' @param pts A SpatialPoints object for the geographic points from which to calculate pairwise distances and paths.
#' @return An object of class SpatialLines
#' @details
#' This function identifies shortest topographic paths from a topographic TransitionLayer.  It does not need to be called separately from the topoDist and topoLCP functions.
#' @examples
#' xy <- matrix(ncol = 2, byrow = TRUE,
#'    c(-119.5566, 37.72474,
#'    -119.4718, 37.76078))
#' xy <- sp::SpatialPoints(xy)
#' topoTL <- topoSurface(Yosemite$DEM)
#' topoPaths(topoTL, xy)
#' @import gdistance
#' @import sp
#' @export
topoPaths <- function(t.dist, pts){
  sp.pairs <- combn(1:length(pts), m = 2)
  pathLines <- list()
  for(i in 1:ncol(sp.pairs)){
    pathLines[[i]] <- shortestPath(t.dist, pts[sp.pairs[1,i]], pts[sp.pairs[2,i]], output = "SpatialLines")
    pathLines[[i]]@lines[[1]]@ID <- paste("Path", sp.pairs[1,i], "-", sp.pairs[2,i], sep = " ")
  }
  paths <- do.call(rbind, pathLines)
  return(paths)
}
