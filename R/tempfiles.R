#' Create a temporary file
#'
#' This is a wrapper around \code{tempfile()} that actually creates the file,
#' to ensure a correctly normalized filepath (i.e., on macOS).
#'
#' @param ext  File extension to give to the newly create file.
#'
#' @return  Character string indicating the filepath to the newly created file.
#'
#' @author Alex Chubaty
#' @export
#' @rdname tf
#'
tf <- function(ext = ".tif") {
  x <- tempfile(fileext = ext)
  file.create(x)
  normalizePath(x, winslash = "/", mustWork = NA)
}
