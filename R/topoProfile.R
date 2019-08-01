#' Elevation profiles of topographic paths
#'
#' Plots topographic cross sections (elevation profiles) of topographic paths
#'
#' @param DEM A RasterLayer for digital elevation model (DEM) data.
#' @param topoPaths A SpatialLines object containing the topographic paths to be plotted.
#' @param pts numeric (default = 100). The number of elevation points to plot along each path.
#' @param type character (default = "base"). Type of plotting: "base" or "plotly".
#' @param singlePlot logical (default = FALSE). If TRUE, profiles will be drawn on a single plot.
#' @param rows numeric (optional). Number of rows for plot layout (if singlePlot = FALSE)
#' @param cols numeric (optional). Number of columns for plot layout (if singlePlot = FALSE)
#' @param limits numeric vector (optional). A vector with the lower and upper limits for the y-axis (elevation).
#' @param legendx character (default = "bottomright"). Position for the legend.
#' @return Plot of elevation profiles
#' @details
#' For the type argument, choosing "base" will use base R plotting, and choosing "plotly" will draw an interactive plot with the plotly package.
#'
#' If rows, cols, or limits are not specified, the most even arrangement will be detected and used.
#'
#' @examples
#' xy <- matrix(ncol = 2, byrow = TRUE,
#'    c(-119.5566, 37.7247,
#'      -119.4718, 37.7608))
#' YosPaths <- topoDist(Yosemite$DEM, xy, paths = TRUE)
#' topoProfile(Yosemite$DEM, topoPaths = YosPaths)
#' @import plotly
#' @importFrom raster extract
#' @import sp
#' @importFrom RColorBrewer brewer.pal
#' @export
topoProfile <- function(DEM, topoPaths, pts = 100, type = "base", singlePlot = FALSE, rows = NULL, cols = NULL,
                        limits = NULL, legendx = "bottomright"){
  if(is.list(topoPaths)) topoPaths <- topoPaths[[2]]
  if(is.null(rows)){
    rows <- cols <- ceiling(sqrt(length(topoPaths)))
  }
  elevations <- list()
  pathDists <- list()
  pathLength <- SpatialLinesLengths(topoPaths, longlat = TRUE)
  for(i in 1:length(topoPaths)){
    samplePts <- spsample(topoPaths[i], n = pts, type = "regular")
    elevations[[i]] <- extract(DEM, samplePts)
    pathDists[[i]] <- seq(0, pathLength[i], by = pathLength[i]/(pts - 1))
  }
  if(is.null(limits)) limits <- c(min(unlist(elevations)) * 0.9, max(unlist(elevations) * 1.1))

  # Base Plotting...
  if(type == "base"){
    if(length(elevations) <= 9) colrs <- brewer.pal(length(elevations), "Set1")
    else colrs <- rainbow(length(elevations))
    if(singlePlot){
      xlimits <- range(unlist(pathDists))
      plot(pathDists[[1]], elevations[[1]], type = "l", lwd = 2, col = colrs[1], xlim = xlimits, ylim = limits,
           main = "Topographic Cross Sections", xlab = "Path Distance", ylab = "Elevation")
      polygon(c(0, pathDists[[1]], pathDists[[1]][length(pathDists[[1]])]),
              c(limits[1], elevations[[1]], limits[1]),
              col = alpha(colrs[1], 0.5), border = NA)
      for(j in 2:length(elevations)){
        lines(pathDists[[j]], elevations[[j]], type = "l", lwd = 2, col = colrs[j])
        polygon(c(0, pathDists[[j]], pathDists[[j]][length(pathDists[[j]])]),
                c(limits[1], elevations[[j]], limits[1]),
                col = alpha(colrs[j], 0.5), border = NA)
      }
      legend(x = legendx, legend = names(topoPaths), pch = 15, col = colrs)
    } else {
      if(is.null(cols)) cols <- 1
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      par(mfrow = c(rows,cols))
      for(j in 1:length(elevations)){
        plot(pathDists[[j]], elevations[[j]], type = "l", lwd = 2,
             xlab = "Path Distance", ylab = "Elevation")
        polygon(c(0, pathDists[[j]], pathDists[[j]][length(pathDists[[j]])]),
                c(limits[1], elevations[[j]], limits[1]),
                col = colrs[j], border = NA)
      }
    }

    # Plotly Plotting...
  } else if(type == "plotly"){
    if(singlePlot){
      path.all <- plot_ly()
      for(j in 1:length(elevations)){
        vals <- data.frame(x = pathDists[[j]], y = elevations[[j]])
        path.all <- add_trace(path.all, x = ~x, y = ~y, data = vals, type = 'scatter', mode = 'lines',
                              fill = 'tozeroy', name = names(topoPaths)[j])
      }
      path.all <- layout(path.all, title = "Topographic Cross Sections",
                         xaxis = list(title = "Path Distance"),
                         yaxis=list(title = "Elevation", range = limits))
    } else {
      path.cs <- list()
      for(j in 1:length(elevations)){
        vals <- data.frame(x = pathDists[[j]], y = elevations[[j]])
        path.cs[[j]] <- plot_ly(vals, x = ~x, y = ~y, type = 'scatter', mode = 'lines',
                                fill = 'tozeroy', name = names(topoPaths)[j])
        path.cs[[j]] <- layout(path.cs[[j]],
                               xaxis = list(title = "Path Distance"),
                               yaxis = list(title = "Elevation", range = limits),
                               annotations = list(x = 0 , y = 1, text = names(topoPaths)[j], showarrow = F,
                                                  xref = "paper", yref = "paper", xanchor = "left"))
      }
      path.all <- subplot(path.cs, nrows = rows, margin = 0.1, titleY = TRUE, titleX = TRUE)
    }
    return(path.all)
  } else {
    message("Type must be 'base' or 'plotly' only.")
  }
}

