#' Convert \code{data.table} objects to \code{RasterLayer} for plotting etc.
#'
#' @param dt   A \code{data.table} object.
#'
#' @param r    A \code{RasterLayer} object.
#'
#' @param val  A string denoting data.table column name to use for raster values.
#'
#' @author ALex Chubaty
#' @export
#' @importClassesFrom data.table data.table
#' @importFrom data.table data.table setkey
#' @importFrom raster cellFromXY ncell
#' @importFrom sp SpatialPoints
dt2raster <- function(dt, r, val) {
  stopifnot(is(dt, "data.table"),
            all(c("ID", "X", "Y") %in% colnames(dt)),
            is(r, "Raster"),
            is.character(val))

  xy <- SpatialPoints(cbind(dt$X, dt$Y))
  ids <- cellFromXY(r, xy)
  tmp <- data.table(ID  = ids, VALUE = dt[[val]])
  tmp <- tmp[, VALUE := sum(VALUE), by = ID]
  setkey(tmp, ID)

  rout <- r
  if (length(tmp$ID)) rout[tmp$ID] <- tmp$VALUE
  if (ncell(rout) - length(tmp$ID) > 0) rout[!tmp$ID] <- NA
  return(rout)
}
