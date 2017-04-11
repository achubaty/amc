################################################################################
#' Do spatial objects intersect?
#'
#' Verifies whether the extent of two spatial objects intersect.
#'
#' @param x     A \code{RasterLayer} object or the name of one.
#' @param layer A \code{RasterLayer} object or a \code{SpatialPolygonDataFrame} object.
#'
#' @return Logical. \code{TRUE} if the intersect does occur between the layers.
#'
#' @docType methods
#' @export
#' @importFrom raster crop extent raster
#'
#' @author Melina Houle
#' @examples
#' if (require(maptools)) {
#'   library(raster)
#'   library(maptools)
#'   data("wrld_simpl")
#'
#'   # From scratch, random raster
#'   xy <- matrix(rnorm(2500), 50, 50)
#'   inputrast <- raster(xy)
#'
#'   # Set Canada extent and projection
#'   extent(inputrast) <- c(-170, -15, 25, 100)
#'   projection(inputrast) <- CRS("+proj=longlat +datum=WGS84")
#'
#'   # Extract Canada boundary from wrld_simpl
#'   canada <- wrld_simpl[which(wrld_simpl@data$NAME == "Canada"),]
#'
#'   isIntersect(inputrast, canada)
#' }
isIntersect <- function(x, layer) {
  if (is(layer,"SpatialPolygonsDataFrame")) {
    lExtent <- extent(raster(layer))
  } else if (is(layer,"RasterLayer")) {
    lExtent <- extent(layer)
  }
  i <- tryCatch(!is.null(crop(x, lExtent)), error = function(e) return(FALSE))
  if (!i) message("Warning: The two layers do not overlap")
  return(i)
}
