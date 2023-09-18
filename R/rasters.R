utils::globalVariables(c("ID", "VALUE"))

#' Convert `data.table` to a `RasterLayer` for plotting, etc.
#'
#' @param dt  `data.table` object with columns `ID`, or both `X`
#'            and `Y`, and the values to assign to the raster specified by
#'            column `val`.
#'
#' @param r   `Raster*` object.
#'
#' @param val The name of the column in `dt` containing the values for the raster.
#'
#' @return A `RasterLayer` object.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom data.table ':=' data.table setkey
#' @importFrom sp SpatialPoints
#' @importFrom raster cellFromXY ncell
#' @rdname dt2raster
#'
#' @examples
#' library(data.table)
#' library(sp)
#' library(raster)
#'
#' r <- raster(nrows = 10, ncols = 10)
#' r[] <- 10
#'
#' # using x,y coordinates
#' #dt1 <- data.table(X = , Y = , value = r[])
#'
#' # using pixel ids
#' dt2 <- data.table(ID = 1L:ncell(r), VALUE = r[])
#' dt2[, VALUE := sample(1L:10L, ncell(r), replace = TRUE)]
#'
#' if (interactive())
#'   plot(dt2raster(dt2, r, "VALUE"))
#'
dt2raster <- function(dt, r, val) {
  stopifnot(is(dt, "data.table"), is(r, "Raster"), is.character(val))

  rout <- r

  if (all(c("X", "Y") %in% colnames(dt))) {
    xy <- SpatialPoints(cbind(dt$X, dt$Y))
    ids <- cellFromXY(r, xy)
    tmp <- data.table(ID  = ids, VALUE = dt[[val]])
    tmp <- tmp[, VALUE := sum(VALUE), by = ID] # nolint
    setkey(tmp, ID)

    if (length(tmp[["ID"]])) rout[tmp[["ID"]]] <- tmp[["VALUE"]]
    if (ncell(rout) - length(tmp[["ID"]]) > 0) rout[!tmp[["ID"]]] <- NA
  } else if ("ID" %in% colnames(dt)) {
    if (length(dt[["ID"]])) rout[dt[["ID"]]] <- dt[[val]]
    if (ncell(rout) - length(dt[["ID"]]) > 0) rout[!dt[["ID"]]] <- NA
  } else {
    stop("dt must have columns X and Y, or column ID.")
  }

  return(rout)
}

#' Merge `Raster*` objects using a function for overlapping areas
#'
#' Provides a wrapper around [raster::mosaic()] that cleans up any
#' temporary intermediate files used, and sets the layer name of the resulting raster.
#'
#' @param x          `Raster*` object
#' @param y          `Raster*` object
#' @param ...        Additional Raster or Extent objects.
#' @param fun        Function (e.g., `mean`, `min`, or `max`, that
#'                   accepts a `na.rm` argument).
#' @param tolerance  Numeric. Permissible difference in origin (relative to the
#'                   cell resolution). See [all.equal()].
#'
#' @param filename   Character. Output filename (optional).
#'
#' @param layerName  Character. Name of the resulting raster layer.
#'
#' @param inRAM      Logical (default `FALSE`) indicating whether function
#'                   operations should be performed in memory or, if `TRUE`,
#'                   using temporary files.
#'
#' @author Alex Chubaty
#' @export
#' @rdname mosaic2
setGeneric("mosaic2", function(x, y, ...) {
  standardGeneric("mosaic2")
})

#' @export
#' @importFrom raster mosaic writeRaster
#' @importFrom stats setNames
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
    out <- raster::mosaic(x, y, ..., fun = fun, tolerance = tolerance,
                          filename = tempfiles[[1]], overwrite = TRUE) |>
      raster::writeRaster(filename = filename, overwrite = TRUE) |>
      stats::setNames(layerName)
    return(out)
})

#' @export
#' @importFrom stats setNames
#' @importFrom terra mosaic
#' @rdname mosaic2
setMethod(
  "mosaic2",
  signature("SpatRaster", "SpatRaster"),
  definition = function(x, y, ..., fun, tolerance = 0.05, filename = NULL,
                        layerName = "layer", inRAM = FALSE) {
    if (missing(filename)) filename <- tf(".tif")
    if (missing(layerName)) layerName <- names(x)

    if (isTRUE(inRAM)) {
      tempfiles <- list("")
    } else {
      tempfiles <- list(tf(".tif"))
      on.exit(unlink(tempfiles))
    }

    ## TO DO: can this part be made parallel?
    out <- terra::mosaic(x, y, ..., fun = fun,
                         filename = tempfiles[[1]], overwrite = TRUE) |>
      terra::writeRaster(filename = filename, overwrite = TRUE) |>
      stats::setNames(layerName)
    return(out)
})
