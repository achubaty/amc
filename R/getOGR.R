#' \code{readOGR} from file not in the current working directory
#'
#' Simply a wrapper for `readOGR` that allows reading a file in a directory
#' that is not the current working directory.
#'
#' @param layer  Layer name. See \code{\link[rgdal]{readOGR}}.
#'
#' @param path   Parent directory of the data source.
#'
#' @param ...    Other arguments passed to \code{readOGR}.
#'
#' @return A Spatial object.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom rgdal readOGR
#' @rdname getOGR
#'
getOGR <- function(layer, path, ...) {
  cwd <- getwd()
  setwd(path); on.exit(setwd(cwd))
  readOGR(dsn = ".", layer = layer, ...)
}
