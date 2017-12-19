utils::globalVariables(c("ID", "VALUE"))

#' Crop and reproject a raster to a given study area
#'
#' This function is geared toward use with study areas that are too large to work
#' with in memory, as it always writes temporary intermediate files to disk.
#' All temporary intermediate files are removed on exit to prevent "disk-full" errors,
#' as normally, the system temp directory is only emptied on reboot.
#'
#' @note Currently, only works with a \code{RasterStack} object from file, and
#' is only tested using \code{studyArea} as a  \code{SpatialPolygonsDataFrame}.
#'
#' @param x          \code{RasterStack} object or the filepath to such object.
#'
#' @param studyArea  A \code{SpatialPolygonsDataFrame} object.
#'
#' @param layerNames A character vector of layer names to assign the raster.
#'
#' @param filename   Optional output filepath to use with \code{\link{writeRaster}}.
#'                   If not specified, a temporary file will be used.
#'
#' @param inRAM      Logical (default \code{FALSE}) indicating whether function
#'                   operations should be performed in memory or, if \code{TRUE},
#'                   using temporary files.
#'
#' @param ...        Additional arguments (not used).
#'
#' @return \code{RasterStack} object (which can be \code{\link[raster]{unstack}}ed).
#'
#' @author Alex Chubaty and Eliot Mcintire
#' @export
#' @importFrom magrittr %>% set_names
#' @importFrom raster crop projectRaster stack writeRaster
#' @importFrom sp CRS proj4string spTransform
#' @rdname cropReproj
#'
setGeneric("cropReproj",
           function(x, studyArea, ...) {
             standardGeneric("cropReproj")
})

#' @export
#' @rdname cropReproj
setMethod(
  "cropReproj",
  signature("RasterStack", "SpatialPolygonsDataFrame"),
  definition = function(x, studyArea, layerNames, filename, inRAM = FALSE, ...) {
    if (missing(filename)) filename <- tf(".grd")
    if (missing(layerNames)) layerNames <- names(x)
    stopifnot(nlayers(x) == length(layerNames))

    if (isTRUE(inRAM)) {
      tempfiles <- list("", "", "")
    } else {
      tempfiles <- lapply(rep(".grd", 3), tf)
      on.exit(lapply(tf, unlink))
    }

    ## TO DO: can this part be made parallel?
    a <- set_names(x, layerNames)
    b <- spTransform(studyArea, CRSobj = CRS(proj4string(a)))
    out <- crop(a, b, filename = tempfiles[[1]], overwrite = TRUE) %>%
      projectRaster(crs = CRS(proj4string(studyArea)), method = "ngb",
                    filename = tempfiles[[2]], overwrite = TRUE) %>%
      crop(studyArea, filename = tempfiles[[3]], overwrite = TRUE) %>%
      set_names(layerNames) %>%
      writeRaster(filename = filename, overwrite = TRUE)

    return(stack(out))
})

#' @export
#' @rdname cropReproj
setMethod(
  "cropReproj",
  signature("RasterStack", "Raster"),
  definition = function(x, studyArea, layerNames, filename, inRAM = FALSE, ...) {
    if (missing(filename)) filename <- tf(".grd")
    if (missing(layerNames)) layerNames <- names(x)
    stopifnot(nlayers(x) == length(layerNames))

    if (isTRUE(inRAM)) {
      tempfiles <- list("", "", "", "")
    } else {
      tempfiles <- lapply(rep(".grd", 4), tf)
      on.exit(lapply(tf, unlink))
    }

    ## TO DO: can this part be made parallel?
    a <- set_names(x, layerNames)
    b <- projectRaster(studyArea, a, method = "ngb",
                       filename = tempfiles[[1]], overwrite = TRUE)
    out <- crop(a, b, filename = tempfiles[[2]], overwrite = TRUE) %>%
      projectRaster(crs = CRS(proj4string(studyArea)), method = "ngb",
                    filename = tempfiles[[3]], overwrite = TRUE) %>%
      crop(studyArea, filename = tempfiles[[4]], overwrite = TRUE) %>%
      set_names(layerNames) %>%
      writeRaster(filename = filename, overwrite = TRUE)

    return(stack(out))
})

#' @export
#' @rdname cropReproj
setMethod(
  "cropReproj",
  signature("RasterLayer", "ANY"),
  definition = function(x, studyArea, layerNames, filename, inRAM = FALSE, ...) {
    cropReproj(stack(x), studyArea, layerNames, filename, inRAM, ...)
})

#' @export
#' @rdname cropReproj
setMethod(
  "cropReproj",
  signature("character", "ANY"),
  definition = function(x, studyArea, layerNames, filename, inRAM = FALSE, ...) {
    stopifnot(file.exists(x))
    cropReproj(stack(x), studyArea, layerNames, filename, inRAM, ...)
})


#' Convert \code{data.table} to a \code{RasterLayer} for plotting, etc.
#'
#' DETAILED DESCRIPTION NEEDED
#'
#' @param dt  \code{data.table} object with columns \code{ID}, \code{X}, \code{Y},
#'            and the values to assign to the raster specified by column \code{val}.
#'
#' @param r   \code{Raster*} object.
#'
#' @param val The name of the column in \code{dt} containing the values for the raster.
#'
#' @return A \code{RasterLayer} object.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom data.table ':=' data.table setkey
#' @importFrom sp SpatialPoints
#' @importFrom raster cellFromXY ncell
#' @rdname dt2raster
#'
# @examples
#'
dt2raster <- function(dt, r, val) {
  stopifnot(is(dt, "data.table"),
            all(c("ID", "X", "Y") %in% colnames(dt)),
            is(r, "Raster"),
            is.character(val))

  xy <- SpatialPoints(cbind(dt$X, dt$Y))
  ids <- cellFromXY(r, xy)
  tmp <- data.table(ID  = ids, VALUE = dt[[val]])
  tmp <- tmp[, VALUE := sum(VALUE), by = ID] # nolint
  setkey(tmp, ID)

  rout <- r
  if (length(tmp$ID)) rout[tmp$ID] <- tmp$VALUE
  if (ncell(rout) - length(tmp$ID) > 0) rout[!tmp$ID] <- NA
  return(rout)
}

#' Merge Raster* objects using a function for overlapping areas
#'
#' Provides a wrapper around \code{\link[raster]{mosaic}} that cleans up any
#' temporary intermediate files used, and sets the layer name of the resulting raster.
#'
#' @param x          \code{Raster*} object
#' @param y          \code{Raster*} object
#' @param ...        Additional Raster or Extent objects.
#' @param fun        Function (e.g., \code{mean}, \code{min}, or \code{max}, that
#'                   accepts a \code{na.rm} argument).
#' @param tolerance  Numeric. Permissible difference in origin (relative to the
#'                   cell resolution). See \code{\link{all.equal}}.
#'
#' @param filename   Character. Output filename (optional).
#'
#' @param layerName  Character. Name of the resulting raster layer.
#'
#' @param inRAM      Logical (default \code{FALSE}) indicating whether function
#'                   operations should be performed in memory or, if \code{TRUE},
#'                   using temporary files.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom magrittr %>% set_names
#' @importFrom raster mosaic writeRaster
#' @rdname mosaic2
setGeneric("mosaic2",
           function(x, y, ...) {
  standardGeneric("mosaic2")
})

#' @export
#' @rdname mosaic2
setMethod(
  "mosaic2",
  signature("RasterLayer", "RasterLayer"),
  definition = function(x, y, ..., fun, tolerance = 0.05, filename = NULL,
                        layerName = "layer", inRAM = FALSE) {
    if (missing(filename)) filename <- tf(".grd")
    if (missing(layerName)) layerName <- names(x)

    if (isTRUE(inRAM)) {
      tempfiles <- list("")
    } else {
      tempfiles <- list(tf(".tif"))
      on.exit(unlink(tempfiles))
    }

    ## TO DO: can this part be made parallel?
    out <- mosaic(x, y, ..., fun = fun, tolerance = tolerance,
                  filename = tempfiles[[1]], overwrite = TRUE) %>%
      writeRaster(filename = filename, overwrite = TRUE) %>%
      set_names(layerName)
    return(out)
})
