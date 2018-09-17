#' Draw convex hull around polygons
#'
#' Draws a convex hull around vertice points of a polygon shapefile,
#' creating a singl polygon.
#'
#' @param x   A \code{SpatialPolygons*} object
#' @param b   Distance to buffer.
#'
#' @return A \code{SpatialPolygons} object.
#'
#' @author Ceres Barros and Alex Chubaty
#' @export
#' @importFrom dismo convHull
#' @importFrom methods as
#' @importFrom raster buffer
#' @importFrom sp polygons proj4string proj4string<- SpatialPoints
#'
outerBuffer <- function(x, b = NULL) {
  stopifnot(is(x, "SpatialPolygons"))
  ## Get polygon vertices
  pts <- SpatialPoints(do.call(rbind, lapply(x@polygons, FUN = function(x1) {
    do.call(rbind, lapply(x1@Polygons, function(x2) x2@coords))
  })))

  ## Draw convex hull around points and extract polygons slot
  hull <- polygons(convHull(pts))
  hull <- as(hull, "SpatialPolygons")
  proj4string(hull) <- proj4string(x)

  ## buffer the polygon
  if (!is.null(b))  {
    hull <- buffer(hull, b)
  }

  return(hull)
}
