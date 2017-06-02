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
#'
#' Sr1 <- Polygon(cbind(c(2, 4, 4, 0.9, 2), c(2, 3, 5, 4, 2)))
#' Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
#' Sr3 <- Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
#'
#' Srs1 <- Polygons(list(Sr1), "s1")
#' Srs2 <- Polygons(list(Sr2), "s2")
#' Srs3 <- Polygons(list(Sr3), "s3")
#' shp <- SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
#' d <- data.frame(vals = 1:3, other = letters[3:1], stringsAsFactors = FALSE)
#' row.names(d) <- names(shp)
#' shp <- SpatialPolygonsDataFrame(shp, data = d)
#' poly <- list()
#' poly[[1]] <- raster(raster::extent(shp), vals = 0, res = c(1, 1))
#' poly[[2]] <- raster(raster::extent(shp), vals = 1, res = c(1, 1))
#' origStack <- stack(poly)
#'
#' # rasterize
#' shpRas1 <- rasterize(shp, origStack, field = "vals")
#' shpRas2 <- fastRasterize(shp, origStack, field = "vals", useGdal = FALSE, datatype = "FLT4S")
#' shpRas3 <- fastRasterize(shp, origStack, field = "vals", useGdal = TRUE, datatype = "FLT4S")
#' shpRas4 <- fastRasterize(shp, origStack, field = "vals", useGdal = TRUE, datatype = "FLT4S",
#'                          filename = "newMap")
#'
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
  suppressWarnings(singleRas[] <- NA_integer_) # for some reason can't handle a raster with all NA
  maskedStack <- stack(lapply(seq_len(nlayers(stack)), function(x) singleRas))
  names(maskedStack) <- names(stack)
  suppressWarnings(maskedStack[nonNACellIDs[, "cell"]] <- croppedStack[nonNACellIDs[, "cell"]])
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
#' @importClassesFrom velox VeloxRaster
#' @importFrom velox velox
#' @importFrom rgdal getGDALVersionInfo
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom raster 'dataType<-' extension fromDisk inMemory setMinMax shapefile raster res tmpDir xmax xmin ymax ymin
#' @importFrom rgdal getGDALVersionInfo
#' @importClassesFrom velox VeloxRaster
#' @importFrom velox velox
#' @rdname faster-rasters
#' @details
#' \code{fastRasterize} will try to keep the object in memory or on disk,
#' depending on whether the input raster was on disk.
#'
fastRasterize <- function(polygon, ras, field, filename, useGdal, datatype) {
  keepInMemory <- inMemory(ras) & missing(filename)

  if(!missing(useGdal)) {
    useGdal <- checkGdalVers2()
  } else {
    useGdal <- FALSE
    if (ncell(ras) > 2e6) {
      useGdal <- checkGdalVers2()
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

  if (missing(field)) field <- names(polygon)

  needFactor <- if (missing(field) | length(field) > 1) {
    TRUE
  } else if (length(field) == 1) {
    is.factor(polygon[[field]]) | is.character(polygon[[field]])
  }

  valsGDAL <- c(16, 32, 64)
  valsRast <- data.frame(vals = c(2^8, 2^16, 2^32, 2^128), labels = c(1, 2, 4, 8))
  intflt <- c("INT", "FLT")

  if (needFactor) {
    int <- 1
    rang <- c(1,NROW(polygon))
    neg <- 2
  } else {
    int <- if (is.integer(polygon[[field]])) 1 else 2
    rang <- range(polygon[[field]])
    neg <- if (rang[1] < 0 & int == 1) 1 else 2
  }

  if (missing(datatype)) {
    if (useGdal) {
      maxV <- which.min(diff(rang) < (2^valsGDAL))
      intflt <- c("Int", "Float")
      datatypeGdal <- paste0("U"[neg], intflt[int], valsGDAL[maxV])
    }
  } else {
    datatypeGdal <- types$gdalTypes[types$rasterTypes %in% datatype]
  }

  if (needFactor) {
    attrNames <- names(polygon)
    if (!("ID" %in% attrNames)) {
      polygon$ID <- 1:NROW(polygon)
    }
  }

  fieldTmp <- if (needFactor) "ID" else field

  if (useGdal) {
    tmpShpFilename <- tempfile(fileext = ".shp")

    # write the polygon object to disk as a shapefile -- can't handle NAs without showing a warning
    suppressWarnings(shapefile(polygon, filename = tmpShpFilename))

    # Run rasterize from gdal
    gdalUtils::gdal_rasterize(a = fieldTmp, tr = res(ras), a_nodata = NA_integer_,
                              tmpShpFilename,
                              te = c(xmin(ras), ymin(ras), xmax(ras), ymax(ras)),
                              rstFilename, ot = datatypeGdal)
    a <- raster(rstFilename)
  } else {
    v1 <- velox(ras) # velox package is much faster than raster package for rasterize function,
    # but not as fast as gdal_rasterize for large polygons
    v1$rasterize(polygon, field = fieldTmp, background = NA_integer_)
    a <- v1$as.RasterLayer(band = 1)
  }

  if (needFactor) {
    whichID <- names(polygon) %in% "ID"
    whichOther <- names(polygon) %in% field
    levels(a) <- data.frame(as.data.frame(polygon[,whichID]),
                            as.data.frame(polygon[,whichOther]))
    names(a) <- "layer"
  } else {
    names(a) <- field
  }

  if (isTRUE(tryCatch(minValue(a), warning = function(x) TRUE)))
    a <- setMinMax(a)

  hasNA <- if (anyNA(a[])) 1 else 0
  maxV <- pmax(pmax(min(which(diff(rang) < (valsRast$vals))), 1 + hasNA), (int == 2)*4)
  if (missing(datatype)) {
    datatype <- paste0(intflt[int], valsRast$labels[maxV], c("U", "S")[neg])
  }

  dataType(a) <- datatype
  if (keepInMemory & fromDisk(a)) a[] <- getValues(a)
  if (!keepInMemory & inMemory(a)) {
    a <- writeRaster(a, filename = file.path(tmpDir(), rstFilename),
                     overwrite = TRUE, datatype = datatype) # need NA value
  }

  return(a)
}


#' fastCrop
#'
#' This function is a wrapper around velox crop function.
#'
#' @export
#' @importFrom raster extent
#' @importClassesFrom velox VeloxRaster
#' @importFrom velox velox
#' @param x Raster to crop
#' @inheritParams raster::crop
#' @details
#' \code{fastRasterize} will try to keep the object in memory or on disk,
#' depending on whether the input raster was on disk.
#'
fastCrop <- function(x, y, ...) {
  v1 <- velox(x) # velox package is much faster than raster package for rasterize function,
  # but not as fast as gdal_rasterize for large polygons
  if(is(y, "Raster")) y <- extent(y)
  v1$crop(y)
  if(length(names(x))>1) {
    a <- v1$as.RasterStack()
  } else {
    a <- v1$as.RasterLayer(band=1)
  }
  a
}


#' Check for gdal version 2
#'
#' Used by many functions in amc
#'
#' @importFrom rgdal getGDALVersionInfo
checkGdalVers2 <-  function() {
  vers <- tryCatch(getGDALVersionInfo(str = "--version"), error = function(x) TRUE)
  useGdal <- FALSE
  if (!isTRUE(vers)) {
    if (as.numeric(substr(strsplit(strsplit(vers, split = ",")[[1]][1], split = " ")[[1]][2], 1, 1)) >= 2)
      useGdal <- TRUE
  }
  return(useGdal)
}
