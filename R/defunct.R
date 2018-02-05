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
  .Defunct("fastMask", package = "SpaDES.tools")
}

#' @export
#' @rdname amc-defunct
#'
fastRasterize <- function() {
  .Defunct("rasterize", package = "fasterize",
           paste("Use fasterize instead.\n",
                 "see https://github.com/ecohealthalliance/fasterize"))
}
