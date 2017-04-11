#' Create a temporory file
#'
#' This is a wrapper around \code{tempfile()} that actually creates the file,
#' to ensure a correctly normalized filepath (i.e., on macOS)..
#'
#' @param ext  Flie extension to give to the nwely create file.
#'
#' @return  A chracter string indicating the filepath to the newly created file.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @rdname tf
#'
tf <- function(ext = ".tif") {
  x <- tempfile(fileext = ext)
  file.create(x)
  normalizePath(x, winslash = "/", mustWork = NA)
}
