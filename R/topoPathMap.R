#' Map of topographic paths
#'
#' Plots a map of topographic paths on a landscape layer
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data.
#' @param pts A SpatialPoints object or two-column matrix with xy coordinates for the geographic points from which to calculate pairwise distances and paths.
#' @param topoPaths A SpatialLines object containing the topographic paths to be plotted.
#' @param type character (default = "hillshade"). Type of map on which to plot topographic paths: "hillshade", "terrain", or "topo".
#' @param costSurface (optional) A RasterLayer for the conductance (inverse of resistance) values for each cell.
#' @param costColors (optional) A function that takes an integer argument (the required number of colors) and returns a character vector of colors (see rgb) interpolating the given sequence (similar to heat.colors or terrain.colors), such as the one returned by colorRampPalette.
#' @param pathWidth numeric (default = 2). Width for drawing path line.
#' @param pathColor character (default = "darkred"). Color for drawing path line.
#' @param alpha numeric (default = 0.65). Alpha transparency for drawing path line.
#' @param angle numeric (default = 45). Angle of lighting for hillshade maps, only (type = "hillshade").
#' @param direction numeric (default = 0). Direction of lighting for hillshade maps, only (type = "hillshade").
#' @param cex numeric (optional). Point size.
#' @param bg character (optional). Background color for spatial points.
#' @param col character (optional). Color for spatial points.
#' @param pch numeric (optional). Shape of spatial points.
#' @param ... character, logical, or numeric (optional). Additional arguments to be passed to the plot function.
#' @return Plot of topographic paths
#' @details
#' The objects supplied for the DEM and pts arguments (and, optionally, costSurface) are generally those used to calculate the topographic paths using topoDist or topoLCP.
#'
#' For the type argument, choosing "hillshade" will plot a shaded relief map, "terrain" will plot a map with terrain colors, and "topo" will plot a map with topo colors.
#'
#' Optional arguments can be supplied to control the size (cex), shape (pch), and color (bg and col) of the points.
#'
#' @examples
#' xy <- matrix(ncol = 2, byrow = TRUE,
#'    c(-119.5566, 37.72474,
#'    -119.4718, 37.76078))
#' YosPaths <- topoDist(Yosemite$DEM, xy, paths = TRUE)
#' topoPathMap(Yosemite$DEM, xy, topoPaths = YosPaths)
#' @importFrom graphics legend lines par points polygon
#' @importFrom grDevices colorRampPalette grey rainbow terrain.colors topo.colors
#' @importFrom raster hillShade ncell terrain
#' @importFrom scales alpha
#' @importFrom utils combn
#' @export
topoPathMap <- function(DEM, pts, topoPaths, type = "hillshade", costSurface = NULL, costColors = NULL,
                        pathWidth = 2, pathColor = "darkred", alpha = 0.65, angle = 45, direction = 0,
                        cex = 2, bg = "gray", col = "black", pch = 21, ...){
  if(is.list(topoPaths)) topoPaths <- topoPaths[[2]]
  if(type == "terrain"){
    plot(DEM, col = terrain.colors(99), ...)
  } else if (type == "topo"){
    plot(DEM, col = topo.colors(99), ...)
  } else if (type == "hillshade"){
    slope <- terrain(DEM, opt = "slope")
    aspect <- terrain(DEM, opt = "aspect")
    hill <- hillShade(slope, aspect, angle = angle, direction = direction)
    plot(hill, col = grey(0:100/100), legend = FALSE, ...)
    if(!is.null(costSurface)){
      if(is.null(costColors)){
        costScheme <- colorRampPalette(c("blue", "green", "yellow", "orange", "red"), space = "rgb", interpolate = "linear")
        costColors <- costScheme(99)
      }
      plot(costSurface, col = costColors, alpha = 0.6, add = TRUE)
    }
  }
  points(pts, pch = pch, bg = bg, cex = cex, col = col)
  lines(topoPaths, col = alpha(pathColor, alpha), lwd = pathWidth)
}
