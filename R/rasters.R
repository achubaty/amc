utils::globalVariables(c("ID", "VALUE"))

#' Convert \code{data.table} to a \code{RasterLayer} for plotting, etc.
#'
#' DETAILED DESCRIPTION NEEDED
#'
#' @param dt  \code{data.table} object with columns \code{ID}, or both \code{X}
#'            and \code{Y}, and the values to assign to the raster specified by
#'            column \code{val}.
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

#' Merge \code{Raster*} objects using a function for overlapping areas
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
