#' Weighted topographic distances and paths
#'
#' Calculates weighted topographic distances and paths
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data; should be in a projected coordinate system.
#' @param pts A SpatialPoints object or two-column matrix with xy coordinates for the geographic points from which to calculate pairwise distances and paths.
#' @param directions numeric (default = 8). The number of directions for movement between cells, either 4 or 8.
#' @param paths logical. Default is FALSE, in which case only topographic distances are calculated.  If TRUE, topographic paths are also identified.
#' @param hFunction character or function (default = NULL). A function describing the cost of changing aspect angle.
#' @param vFunction character or function (default = NULL). A function describing the cost of movement along an incline.
#' @return Matrix of transport costs (if paths = FALSE), or a list containing a matrix of transport costs and paths as an object of class SpatialLines (if paths = TRUE).
#' @details
#' The hFunction argument can be set to "exponential" or "linear" to use standard functions for the cost of aspect angle changes. If providing a custom function instead, the equation should be a function of one variable, angle (in radians).
#' The vFunction argument can be set to "exponential" or "quadratic" to use standard functions for the cost of movement along an incline.  If providing a custom function instead, The equation should be a function of one variable, slope.
#' If paths = FALSE, the function will return a matrix of pairwise topographic distances between the specified points.
#' If paths = TRUE, the function will return a list with two items: (1) the matrix of pairwise topographic distances,
#' and (2) a SpatialLines object containing the topographic paths.
#' @examples
#' xy <- matrix(ncol = 2, byrow = TRUE,
#'              c(-119.5566, 37.7247,
#'                -119.4718, 37.7608))
#' topoWeightedDist(Yosemite$DEM, xy, vFunction = "exponential")
#' @importFrom gdistance geoCorrection transition shortestPath
#' @importFrom raster adjacent projectRaster ncell
#' @importFrom sp SpatialPoints
#' @export
topoWeightedDist <- function(DEM, pts, directions = 8, paths = FALSE, hFunction = NULL, vFunction = NULL){
  pts <- sp::SpatialPoints(pts)
  adj <- raster::adjacent(DEM, cells = 1:ncell(DEM), pairs = TRUE, directions = directions)
  if(!is.null(hFunction)){
    if(is.character(hFunction)){
      if(hFunction == "exponential"){
        hFunction <- function(x){exp(x)}
      }else if(hFunction == "linear"){
        hFunction <- function(x){x+1}
      }
    }
    aspect <- raster::terrain(DEM, opt = "aspect")
    h.func <- function(x){min(abs(x[2] - x[1]), ((2 * pi) - x[2] + x[1]), ((2 * pi) - x[1] + x[2]), na.rm = TRUE)}
    a.dist <- gdistance::transition(aspect, transitionFunction = h.func, directions = 8, symm = FALSE)
    a.dist[adj] <- hFunction(a.dist[adj])
  }
  if(!is.null(vFunction)){
    if(is.character(vFunction)){
      if(vFunction == "exponential"){
        vFunction <- function(x){exp(x)}
      } else if(vFunction == "quadratic"){
        vFunction <- function(x){1+x^2}
      }
    }
    slope <- suppressWarnings(gdistance::transition(DEM, transitionFunction = function(x){x[2] - x[1]}, directions = directions, symm = FALSE))
    slope <- gdistance::geoCorrection(slope)
    s.dist <- slope
    s.dist[adj] <- vFunction(s.dist[adj])
  }
  t.dist <- topoSurface(DEM, conductance = FALSE, directions = directions)
  w.dist <- t.dist
  if(is.null(hFunction) & is.null(vFunction)){
    print("No hFunction or vFunction. Better to just use topoDist().")
  }else if(is.null(hFunction)){
    w.dist[adj] <- 1/(t.dist[adj] * s.dist[adj])
  }else if(is.null(vFunction)){
    w.dist[adj] <- 1/(t.dist[adj] * a.dist[adj])
  }else {
    w.dist[adj] <- 1/(t.dist[adj] * a.dist[adj] * s.dist[adj])
  }
  wDistances <- as.matrix(gdistance::costDistance(w.dist, pts))
  if(paths){
    sp.pairs <- combn(1:length(pts), m = 2)
    pathLines <- list()
    for(i in 1:ncol(sp.pairs)){
      pathLines[[i]] <- gdistance::shortestPath(w.dist, pts[sp.pairs[1,i]], pts[sp.pairs[2,i]], output = "SpatialLines")
      pathLines[[i]]@lines[[1]]@ID <- paste("Path", sp.pairs[1,i], "-", sp.pairs[2,i], sep = " ")
    }
    paths <- do.call(rbind, pathLines)
    return(list(wDistances, paths))
  } else {
    return(wDistances)
  }
}

