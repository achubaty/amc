#' Faster masking of a raster using a polygon
#'
#' Description needed
#'
#' @note HAS NOT BEEN FULLY TESTED
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
#' @examples
#' \dontrun{
#' if (require("SpaDES", quietly = TRUE)) {
#'   library(raster)
#'
#'   beginCluster(2)
#'   r <- raster(extent(0, 15, 0, 15), vals = 0)
#'   poly <- list()
#'   poly[[1]] <- SpaDES::randomPolygons(r, numTypes = 10)
#'   poly[[2]] <- SpaDES::randomPolygons(r, numTypes = 10)
#'   origStack <- stack(poly)
#'
#'   shp <- origStack[[1]]
#'   shp[shp == shp[1]] <- NA # take a chunk out of the raster
#'   shp <- rasterToPolygons(shp, dissolve = TRUE) # convert to polygon
#'
#'   # original mask function in raster
#'   newStack1 <- mask(origStack, mask = shp)
#'
#'   # fastMask uses 2 clusters
#'   newStack2 <- fastMask(stack = origStack, polygon = shp)
#'
#'   # test all equal
#'   identical(newStack1, newStack2)
#'   newStack1 <- stack(newStack1)
#'   newStack2 <- stack(newStack2)
#'   if (interactive()) {
#'     SpaDES::clearPlot()
#'     SpaDES::Plot(newStack2)
#'   }
#'
#'   # rasterize
#'   shpRas1 <- rasterize(shp, origStack)
#'   shpRas2 <- fastRasterize(shp, origStack)
#'   identical(shpRas1, shpRas2)
#'
#'   if (interactive()) {
#'     SpaDES::clearPlot()
#'     SpaDES::Plot(shpRas2)
#'   }
#'
#'   endCluster()
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

#' Faster rasterizing of a polygon
#'
#' Description needed.
#'
#' @note HAS NOT BEEN FULLY TESTED
#'
#' @param ras        A \code{RasterLayer} object.
#'
#' @param field      The field to use from \code{polygon}.
#'
#' @return A \code{Raster*} object.
#'
#' @author Eliot Mcintire
#' @docType methods
#' @export
#' @importFrom plyr mapvalues
#' @importFrom raster extract raster
#' @rdname faster-rasters
#'
fastRasterize <- function(polygon, ras, field) {
  nonNACellIDs <- extract(ras, polygon, cellnumbers = TRUE)
  polygonIDs <- seq_along(nonNACellIDs)
  nonNACellIDs <- lapply(polygonIDs, function(x) cbind(nonNACellIDs[[x]], "ID" = x))
  nonNACellIDs <- do.call(rbind, nonNACellIDs)
  singleRas <- raster(ras)
  singleRas[] <- NA
  singleRas[nonNACellIDs[, "cell"]] <- nonNACellIDs[, "ID"]
  if (!missing(field)) {
    if (length(field) == 1) {
      singleRas[] <- mapvalues(singleRas[], from = polygonIDs, to = polygon[[field]])
      numFields <- 1
    } else {
      numFields <- 2
    }
  } else {
    numFields <- 3
  }
  if (numFields == 3) {
    field <- names(polygon)
  }
  levels(singleRas) <- data.frame(ID = polygonIDs, polygon[field])
  singleRas
}
