#' Faster operations on rasters with internal parallel aware
#'
#' These alternatives to \code{mask} and \code{rasterize} are not as general as
#' the originals. However, they use \code{raster::extract} internally,
#' which is parallel-aware. So, using these functions with a cluster
#' having been created via \code{beginCluster} will be much faster
#' than mask and rasterize. However, only a few situations will work
#' with these functions (e.g., \code{fastMask} must be given a stack --
#' a Raster will likely not work)
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
#' @rdname fasterRaster
#' @examples
#'\dontrun{
#' library(raster)
#' library(sp)
#' beginCluster(2)
#'
#' Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
#' Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
#' Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
#'
#' Srs1 = Polygons(list(Sr1), "s1")
#' Srs2 = Polygons(list(Sr2), "s2")
#' Srs3 = Polygons(list(Sr3), "s3")
#' shp = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
#' d <- data.frame(vals=  1:3, other = letters[1:3])
#' row.names(d) <- names(shp)
#' shp <- SpatialPolygonsDataFrame(shp, data = d)
#' poly <- list()
#' poly[[1]] <- raster(extent(shp), vals = 0, res = c(0.5,0.5))
#' poly[[2]] <- raster(extent(shp), vals = 1, res = c(0.5,0.5))
#' origStack <- stack(poly)
#'
#' # rasterize
#' shpRas1 <- rasterize(shp, origStack)
#' shpRas2 <- fastRasterize(shp, origStack)
#' all.equal(shpRas1, shpRas2)
#'
#' if(interactive()) {
#'   plot(shpRas2)
#' }
#'
#' # original mask function in raster
#' newStack1 <- mask(origStack, mask=shp)
#' # fastMask uses 2 clusters
#' newStack2 <- fastMask(stack = origStack, polygon = shp)
#'
#' # test all equal
#' identical(newStack1, newStack2)
#' newStack1 <- stack(newStack1)
#' newStack2 <- stack(newStack2)
#' if(interactive()) {
#'   plot(newStack2[[1]])
#'   plot(shp, add=TRUE)
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
#' @author Eliot Mcintire
#' @docType methods
#' @export
#' @importFrom plyr mapvalues
#' @importFrom raster extract raster
#' @rdname fasterRaster
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
