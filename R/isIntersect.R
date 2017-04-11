##################################################################################
#' Do SPatial objects intersect
#'
#' is Intersect verifies if the extent of two Spatial objects are intersecting. Returns TRUE if its argument intersect.
#' This function was created to avoid empty output created mask() function in dataPrepRaster. Mask need the layers
#'
#' @param x   A \code{RasterLayer} object or the name of one.
#' @param layer A RasterLayer object or a SpatialPolygonDataFrame object.
#'
#' @return Logical TRUE if the intersect does occur between the 2 layers.
#'
#' @importFrom raster extent crop raster
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' library(raster)
#' library(maptools)
#' data("wrld_simpl")
#'
#' # Set
#' outputPath <- tempdir()
#'
#' #From scratch, random raster
#' xy <- matrix(rnorm(2500),50,50)
#' inputrast <- raster(xy)
#'
#' #Set Canada extent and projection
#' extent(inputrast) <- c(-170,-15,25,100)
#' projection(inputrast) <- CRS("+proj=longlat +datum=WGS84")
#'
#' #Extract Canada boundary from wrld_simpl
#' canada<-wrld_simpl[which(wrld_simpl@data$NAME =='Canada'),]
#'
#' isIntersect(inputrast,canada)
isIntersect<-function(x, layer){
  if (is(layer,"SpatialPolygonsDataFrame")) lExtent <-extent(raster(layer))
  if (is(layer,"RasterLayer")) lExtent <-extent(layer)
  i<-tryCatch(!is.null(crop(x,lExtent)), error=function(e) return(FALSE))
  if(!i)  message("Warning: The two layers do not overlap")
  return(i)
}
