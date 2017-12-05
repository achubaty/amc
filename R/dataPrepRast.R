#' Reproject, crop and mask a raster map
#'
#' This script reprojects, crop and mask a raster using a Raster* object or a SpatialPolygon object.
#'
#' @param x   A \code{RasterLayer} object or the name of one.
#'
#' @param crop   Raster*, SpatialPolygons*, Extent* object from which an Extent can
#'                   be extracted (see Details of \code{\link[raster]{crop}}).
#'
#' @param mask   Raster* or SpatialPolygons* object passed to \code{\link[raster]{mask}}.
#'
#' @param reprojectTo   A character or object of class 'CRS' that is used to set the output projection.
#'
#' @param outPath   Path where filename is saved.
#'
#' @param filename   Output filename to write to (\code{\link[raster]{writeRaster}}).
#'
#' @param format File format to write out (\code{\link[raster]{writeRaster}}).
#'                   If not specified, GTiff will be used.
#'
#' @param timings   Logical indicating whether Sys.time messages should be
#'                   suppressed. Default is \code{FALSE}.
#'
#' @param ...   Additional arguments to the aforementioned \code{raster} functions.
#'
#' @return A \code{RasterLayer} object.
#'
#' @importFrom raster compareCRS crop crs mask projectRaster writeRaster extent projectExtent compareRaster
#' @importFrom sp spTransform
#' @importFrom magrittr set_names
#' @importFrom utils capture.output
#' @docType methods
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
#'   canada <- wrld_simpl[which(wrld_simpl@data$NAME == 'Canada'),]
#'
#'   # dataPrepRast
#'   x <- dataPrepRast(inputrast, crop = canada , mask = canada,
#'                     reprojectTo = inputrast, timings = TRUE)
#' }
setGeneric("dataPrepRast",
           function(x, crop, ...) {
             standardGeneric("dataPrepRast")
           })

#' @export
#' @rdname dataPrepRast
setMethod(
  "dataPrepRast",
  signature("RasterLayer", "RasterLayer"),
  definition = function(x, crop, mask= NULL, reprojectTo = NULL, outPath = NULL,
                        filename = NULL, format = "GTiff", timings = FALSE, ...) {
    cwd <- getwd()
    if (!is.null(outPath)) {
      setwd(outPath)
      on.exit(setwd(cwd))
    }
    if(missing(mask)) mask <- NULL
    if(missing(reprojectTo)) reprojectTo <- NULL

    if (is.na(crs(x)))
        stop("Input object must have a coordinate system assigned.")
    ## PRE-CROP
    if (!is.null(crop)) {
      if(is.na(crs(crop))) stop("Crop object must have a coordinate system assigned.")

      if (timings) preTime <- Sys.time()
      # Reproject crop if CRS don't match
      if(!compareCRS(x, crop)){
        crop <- projectExtent(crop, crs= crs(x))
      }
      # crop
      x <- crop(x, crop)
      if (timings) {
        message(paste("finished croping...took ", format(Sys.time() - preTime, digits = 2)))
      }
    }

    # SET OUTPUT PROJECTION.
    if (!is.null(reprojectTo)) {
      reprojectTo <- crs(reprojectTo)
      if(!compareCRS(x, reprojectTo)){
        x <- projectRaster(x, crs = reprojectTo)
      }
    }
    # CROP
    if (!is.null(crop)){
      if(!compareCRS(x, crop)){
        crop <- projectExtent(crop, crs= crs(x))
      }
     x <- crop(x, crop)
    }

   # MASK
   if (!is.null(mask)) {
     if(is.na(crs(mask)))  stop("Mask object must have a coordinate system assigned.")
     if (timings) preTime <- Sys.time()
     if (is(mask, "SpatialPolygonsDataFrame")) {
       if(!compareCRS(x, mask)) {
          mask <- spTransform(mask, crs(x))
       }
     }else{
       if(!compareRaster(x, mask, stopiffalse=FALSE)){
          mask <- projectRaster(mask, x, method = "ngb")
       }
     }

    # check if mask intersect to avoid empty output
    noIntersect<-is.null(raster::intersect(extent(x), extent(mask)))
    # Make it fail if no intersect
    if(noIntersect) stop("Input does not intersect mask")

    # mask
    x <- mask(x, mask = mask)
    if (timings) {
      message(paste("finished masking...took ", format(Sys.time() - preTime, digits = 2 )))
    }
   }

  if (!is.null(filename)) {
    writeRaster(x, filename = filename, format = format, overwrite = TRUE)
  }
  return(x)
})

#' @export
#' @rdname dataPrepRast
setMethod(
    "dataPrepRast",
    signature("RasterLayer", "SpatialPolygonsDataFrame"),
    definition = function(x, crop, mask, reprojectTo,
                          outPath = NULL, filename = NULL, format = "GTiff",
                          timings = FALSE, ...) {
      if(is.na(crs(crop))) stop("Crop object must have a coordinate system assigned.")

      crop <- spTransform(crop, crs(x))
      crop <- raster(extent(crop), crs=crs(x), val=1)
      dataPrepRast(x, crop = crop, mask, reprojectTo, outPath = NULL,
                    filename = NULL, format = "GTiff", timings = FALSE, ...)
})

#' @export
#' @rdname dataPrepRast
setMethod(
  "dataPrepRast",
  signature("RasterLayer", "Extent"),
  definition = function(x, crop, mask, reprojectTo, outPath = NULL, filename = NULL,
                        format = "GTiff", timings = FALSE, ...) {

    if (is.na(crs(x)))
      stop("Input object must have a coordinate system assigned.")

    crop <- raster(crop, crs=crs(x))
    dataPrepRast(x, crop = crop, mask, reprojectTo, outPath = NULL,
                 filename = NULL, format = "GTiff", timings = FALSE, ...)
})

#' @export
#' @rdname dataPrepRast
setMethod(
  "dataPrepRast",
  signature(x= "RasterLayer", crop= "missing"),
  definition = function(x, crop, mask, reprojectTo, outPath = NULL, filename = NULL,
                        format = "GTiff", timings = FALSE, ...) {

    cwd <- getwd()
    if (!is.null(outPath)) {
      setwd(outPath)
      on.exit(setwd(cwd))
    }
    if(missing(mask)) mask <- NULL
    if(missing(reprojectTo)) reprojectTo <- NULL

    if (is.na(crs(x)))
      stop("Input object must have a coordinate system assigned.")

    # SET OUTPUT PROJECTION.
    if (!is.null(reprojectTo)) {
      reprojectTo <- crs(reprojectTo)
      if(!compareCRS(x,reprojectTo)){
        x <- projectRaster(x, crs = reprojectTo)
      }
    }
    # MASK
    if (!is.null(mask)) {
      if(is.na(crs(mask)))  stop("Mask object must have a coordinate system assigned.")
      if (timings) preTime <- Sys.time()
      if (is(mask, "SpatialPolygonsDataFrame")) {
        if(!compareCRS(x, mask)) {
          mask <- spTransform(mask, crs(x))
        }
      }else{
        # Fix projection or extent.
        if(!compareRaster(x, mask, stopiffalse=FALSE)){
          mask <- projectRaster(mask, x, method = "ngb")
        }
      }

      # check if mask intersect to avoid empty output
      noIntersect<-is.null(raster::intersect(extent(x), extent(mask)))
      # Make it fail if no intersect
      if(noIntersect) stop("Input does not intersect mask object")

      # mask
      x <- mask(x, mask = mask)
      if (timings) {
        message(paste("finished masking...took ", format(Sys.time() - preTime, digits = 2 )))
      }
    }

    if (!is.null(filename)) {
      writeRaster(x, filename = filename, format = format, overwrite = TRUE)
    }
    return(x)
  })

