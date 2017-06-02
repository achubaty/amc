#' Faster operations on rasters
#'
#' These alternatives to \code{mask} and \code{rasterize} are not as general as
#' the originals. \code{fastRasterize} uses either \code{velox} package or,
#' if \code{gdal} is installed and accessible by \code{rgdal::getGDALVersionInfo}
#' and the version is > 2.0, then it will default to \code{gdalUtils::gdal_rasterize}.
#' This default will be overridden for "small" rasters (fewer than 2e+6 cells),
#' as \code{velox} is faster in those cases.
#' The user can specify whether to use \code{GDAL} with the \code{useGdal} argument.
#' For \code{fastMask}, the function uses use \code{raster::extract} internally,
#' which is parallel-aware. So, using this function with a cluster having been
#' created via \code{beginCluster} will be much faster than \code{mask}.
#'
#' @note This is experimental and not all combinations of parameters or object
#' types will work, e.g., \code{fastMask} must be given a \code{RasterStack}.
#'
#' @param stack      A \code{RasterStack} object.
#'
#' @param polygon    A \code{SpatialPolygons} object.
#'
#' @return A \code{Raster*} object.
#'
#' @author Eliot Mcintire
#' @docType methods
#' @export
#' @importFrom raster crop extract nlayers raster stack
#' @rdname faster-rasters
#'
#' @examples
#'\dontrun{
#' library(raster)
#' library(sp)
#'
#' Sr1 <- Polygon(cbind(c(2, 4, 4, 0.9, 2), c(2, 3, 5, 4, 2)))
#' Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
#' Sr3 <- Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
#'
#' Srs1 <- Polygons(list(Sr1), "s1")
#' Srs2 <- Polygons(list(Sr2), "s2")
#' Srs3 <- Polygons(list(Sr3), "s3")
#' shp <- SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
#' d <- data.frame(vals = 1:3, other = letters[1:3])
#' row.names(d) <- names(shp)
#' shp <- SpatialPolygonsDataFrame(shp, data = d)
#' poly <- list()
#' poly[[1]] <- raster(extent(shp), vals = 0, res = c(1, 1))
#' poly[[2]] <- raster(extent(shp), vals = 1, res = c(1, 1))
#' origStack <- stack(poly)
#'
#' # rasterize
#' shpRas1 <- raster::rasterize(shp, origStack, field = "vals")
#' shpRas2 <- fastRasterize(shp, origStack, field = "vals", useGdal = FALSE, datatype = "FLT4S")
#' shpRas3 <- fastRasterize(shp, origStack, field = "vals", useGdal = TRUE, datatype = "FLT4S")
#' if (require("testthat")) {
#'   expect_equal(shpRas1, shpRas2)
#'   expect_equal(shpRas1, shpRas3)
#' }
#'
#' if (interactive()) plot(shpRas2)
#'
#' # original mask function in raster
#' newStack1 <- mask(origStack, mask = shp)
#'
#' # fastMask uses cluster
#' newStack2 <- fastMask(stack = origStack, polygon = shp)
#'
#' # test all equal
#' identical(newStack1, newStack2)
#' newStack1 <- stack(newStack1)
#' newStack2 <- stack(newStack2)
#'
#' if (interactive()) {
#'   plot(newStack2[[1]])
#'   plot(shp, add = TRUE)
#' }
#' }
#'
fastMask <- function(stack, polygon) {
  croppedStack <- crop(stack, polygon)
  nonNACellIDs <- extract(croppedStack[[1]], polygon, cellnumbers = TRUE)
  nonNACellIDs <- do.call(rbind, nonNACellIDs)
  singleRas <- raster(croppedStack[[1]])
  singleRas[] <- NA
  maskedStack <- stack(lapply(seq_len(nlayers(stack)), function(x) singleRas))
  names(maskedStack) <- names(stack)
  maskedStack[nonNACellIDs[, "cell"]] <- croppedStack[nonNACellIDs[, "cell"]]
  maskedStack
}

#' @param ras        A \code{RasterLayer} object.
#'
#' @param field      The field to use from \code{polygon}.
#'
#' @param filename Character string giving the filename. Note: if \code{filename}
#'                 is supplied, only the basename of the file is used, and the
#'                 the output raster will be saved using \code{.tif} format.
#'
#' @param useGdal Logical. If missing (default), the function will
#'
#' @param datatype Passed to raster object and disk-format. See \code{\link[raster]{dataType}}
#'
#' @export
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom raster 'dataType<-' extension inMemory setMinMax shapefile raster res tmpDir xmax xmin ymax ymin
#' @importFrom rgdal getGDALVersionInfo
#' @importClassesFrom velox VeloxRaster
#' @importFrom velox velox
#' @rdname faster-rasters
#' @details
#' \code{fastRasterize} will try to keep the object in memory or on disk,
#' depending on whether the input raster was on disk.
#'
fastRasterize <- function(polygon, ras, field, filename, useGdal, datatype) {
  keepInMemory <- inMemory(ras)
  if (missing(useGdal)) {
    useGdal <- FALSE
    if (ncell(ras) > 2e6) {
      vers <- tryCatch(getGDALVersionInfo(str = "--version"), error = function(x) TRUE)
      if (!isTRUE(vers)) {
        if (as.numeric(substr(strsplit(strsplit(vers, split = ",")[[1]][1], split = " ")[[1]][2], 1, 1)) >= 2)
          useGdal <- TRUE
      }
    }
  }
  rstFilename <- if (!missing(filename)) {
    extension(basename(filename), ".tif")
  } else {
    tempfile(fileext = ".tif")
  }

  # These are taken from ?dataType from the raster package and http://www.gdal.org/frmt_gtiff.html
  types <- data.frame(rasterTypes = c("INT1S", "INT1U", "INT2S", "INT2U", "INT4S", "INT4U", "FLT4S", "FLT4U"),
                      gdalTypes = c("16Int", "U16Int", "16Int", "U16Int","32Int", "U32Int", "32Float", "32Float"),
                      stringsAsFactors = FALSE)

  if (missing(datatype)) {
    int <- if (is.integer(polygon[[field]])) 1 else 2
    rang <- range(polygon[[field]])
    valsGDAL <- c(16, 32, 64)
    valsRast <- data.frame(vals = c(2^8, 2^16, 2^32, 2^128), labels = c(1, 2, 4, 8))
    neg <- if (rang[1] < 0 & int == 1) 1 else 2
    hasNA <- anyNA(ras[])

    if (isTRUE(useGdal)) {
      maxV <- which.min(diff(rang) < (2^valsGDAL))
      intflt <- c("Int", "Float")
      datatypeGdal <- paste0("U"[neg], intflt[int], valsGDAL[maxV])
    }
    maxV <- pmax(pmin(min(which(diff(rang) < (valsRast$vals))), 1 + hasNA), (int == 2) * 4)
    intflt <- c("INT", "FLT")
    datatype <- paste0(intflt[int], valsRast$labels[maxV], c("U", "S")[neg])
  } else {
    datatypeGdal <- types$gdalTypes[types$rasterTypes %in% datatype]
  }

  if (isTRUE(useGdal)) {
    tmpShpFilename <- tempfile(fileext = ".shp")

    # write the polygon object to disk as a shapefile
    shapefile(polygon, filename = tmpShpFilename)

    # Run rasterize from gdal
    gdalUtils::gdal_rasterize(a = field, tr = res(ras), a_nodata = NA_integer_,
                              tmpShpFilename,
                              te = c(xmin(ras), ymin(ras), xmax(ras), ymax(ras)),
                              rstFilename, ot = datatypeGdal)
    a <- raster(rstFilename)
    if (keepInMemory) a[] <- getValues(a)
    a <- setMinMax(a)
  } else {
    # velox package is much faster than raster package for rasterize function,
    # but not as fast as gdal_rasterize for large polygons
    v1 <- velox::velox(ras)
    v1$rasterize(polygon, field = field, background = NA_integer_)
    a <- v1$as.RasterLayer(band = 1)
    if (!keepInMemory | !missing(filename)) {
      a <- writeRaster(a, filename = file.path(tmpDir(), rstFilename),
                       overwrite = TRUE, datatype = datatype) # need NA value
    }
  }
  if (keepInMemory) dataType(a) <- datatype

  return(a)
}
