#' Crop, reproject, and mask a raster map
#'
#' This script reprojects and crop a raster using a raster or polygon object.
#'
#' @param x   A \code{RasterLayer} object or the name of one.
#'
#' @param cropWith   Extent object, or any object from which an Extent object can
#'                   be extracted (see Details of \code{\link[raster]{crop}}).
#'
#' @param maskWith   Raster* or Spatial object passed to \code{\link[raster]{mask}}.
#'
#' @param reprojectTo   Raster* or Spatial object passed to \code{\link[raster]{crs}}.
#'
#' @param outPath   Path where fileOut is saved.
#'
#' @param fileOut   File name to write to (\code{\link[raster]{writeRaster}}).
#'
#' @param formatOut File format to write out (\code{\link[raster]{writeRaster}}).
#'                   If not specified, GTiff will be used.
#'
#' @param timings   Logical indicating whether Sys.time messages should be
#'                   suppressed. Default is \code{FALSE}.
#'
#' @param ...   Additional arguments to the aforementioned \code{raster} functions.
#'
#' @return A \code{RasterLayer} abject.
#'
#' @importFrom raster compareCRS crop crs mask projectRaster writeRaster
#' @importFrom sp spTransform
#' @importFrom magrittr set_names
#' @importFrom utils capture.output
#' @export
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
#'   x <- dataPrepRast(inputrast, cropWith = canada , maskWith = canada,
#'                     reprojectTo = inputrast, timings = TRUE)
#' }
#'
dataPrepRast <- function(x, cropWith = NULL, maskWith = NULL, reprojectTo = NULL,
                         outPath = NULL, fileOut = NULL, formatOut = "GTiff",
                         timings = FALSE, ...) {
  cwd <- getwd()
  if (!is.null(outPath)) {
    setwd(outPath)
    on.exit(setwd(cwd))
  }
  if (!is.null(cropWith)) cropWithname <- deparse(substitute(cropWith))
  if (!is.null(maskWith)) maskWithname <- deparse(substitute(maskWith))
  xname <- deparse(substitute(x))

  # Set input list with names
  inputls <- list(x, cropWith, maskWith, reprojectTo) %>%
    set_names(c("x", "cropWith", "maskWith", "reprojectTo"))

  # remove NULL
  ilst <- inputls[!sapply(inputls, is.null)]

  # set output projection. If not defined, derive reprojectTo using crs of param x
  if (is.null(reprojectTo)) {
    if (!is.na(crs(ilst$x))) {
      reprojectTo <- crs(ilst$x)
    } else {
      stop(paste("You must provide a coordinate system (reprojectTo) or",
                 "have your input object assigned to a coordinate reference."))
    }
  } else {
    reprojectTo <- crs(ilst$reprojectTo)
  }

  # reproject all inputs into the same CRS
  ilst <- lapply(ilst, function(i) {
    if (is(i, "SpatialPolygonsDataFrame")) {
      if (!compareCRS(i, reprojectTo)) {
        i <- spTransform(i, reprojectTo)
      }
    } else if (is(i, "RasterLayer")) {
      if (!compareCRS(i, reprojectTo)) {
        i <- projectRaster(i, crs = reprojectTo)
      }
    } else {
      stop("object class is not recognized")
    }
    return(i)
  })

  # crop
  if (!is.null(cropWith)) {
    if (timings) preTime <- Sys.time()
    # Test if x and maskWith intersect.
    # To avoid empty output if extents do not intersect while using rasterize()
    doIntersect <- isIntersect(ilst$x, ilst$cropWith)
    if (doIntersect) {
      ilst$x <- crop(ilst$x, ilst$cropWith)
      if (timings) {
        msg <- capture.output(
          print(paste("finished croping ", xname, "with", cropWithname)),
          print(paste("took", format(Sys.time() - preTime, digits = 2)))
        )
        message(msg)
      }
    } else {
      message("Crop did not occur.")
    }
  }

  # mask
  if (!is.null(maskWith)) {
    if (timings) preTime <- Sys.time()
    # Test if x and maskWith intersect.
    # To avoid empty output if extents do not intersect while using rasterize()
    doIntersect <- isIntersect(ilst$x, ilst$maskWith)
    if (doIntersect) {
      ilst$x <- mask(ilst$x, mask = ilst$maskWith)
      if (timings) {
        msg <- capture.output(
          print(paste("finished masking ", xname, "with", maskWithname)),
          print(paste("took", format(Sys.time() - preTime, digits = 2)))
        )
        message(msg)
      }
    } else {
      message("Mask did not occur.")
    }
  }

  if (!is.null(fileOut)) {
    writeRaster(ilst$x, filename = fileOut, format = formatOut, overwrite = TRUE)
  }
  return(invisible(ilst$x))
}
