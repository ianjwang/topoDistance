#' topoDistance: A package for calculating topographic paths and distances
#'
#' The topoDistance package provides functions for calculating topographic distances and
#' identifying and plotting topographic paths. Topographic distances can be
#' calculated along shortest topographic paths or topographic least cost paths.
#' Functions can map topographic paths on colored or hillshade maps and plot
#' topographic cross sections (elevation profiles) for the paths.
#'
#' Unlike the topographically-corrected distances calculated by some GIS software,
#' which just adjust for elevational changes along a straight-line path between points,
#' topoDistance calculates the distance along the shortest topographic path between points,
#' which is more likely to realistically reflect biological movement on a topographically
#' complex landscape.
#'
#' Topographic distances are calculated as the hypotenuse of the horizontal and vertical
#' distances between cells on an elevation raster.  These distances are assignd to the
#' weights of vertices between the nodes for each cell on a landscape graph, and functions
#' from the gdistance and igraph packages are used to find the shortest path between nodes.
#' For topographic least cost paths, resistance distance weights are multiplied by the
#' topographic distance weights to get topographically corrected least cost path distances.
#'
#' @docType package
#' @name topoDistance
NULL
