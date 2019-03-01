#' Defunct functions
#'
#' These functions have been removed.
#'
#' @export
#' @rdname amc-defunct
#'
fastCrop <- function() {
  .Defunct("fastCrop", package = "SpaDES.tools")
}

#' @export
#' @rdname amc-defunct
#'
fastMask <- function() {
  .Defunct("fastMask", package = "reproducible")
}

#' @export
#' @rdname amc-defunct
#'
fastRasterize <- function() {
  .Defunct("rasterize", package = "fasterize",
           paste("Use fasterize instead.\n",
                 "see https://github.com/ecohealthalliance/fasterize"))
}

#' @export
#' @rdname amc-defunct
#'
loadPackages <- function() {
  .Defunct("loadPackages", package = "SpaDES.core")
}
