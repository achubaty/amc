#' Temporary directory and file creation
#'
#' These are wrappers around \code{tempdir} and \code{tempfile} that creates the
#' directory or file, to ensure a correctly normalized filepath (i.e., on macOS).
#'
#' @param dir  Path to use as temporary directory. A subdirectory will be created.
#'             Default is to use the R session temporary directory.
#' @param ext  File extension to give to the newly create file.
#'
#' @return  Character string indicating the filepath to the newly created file.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom reproducible checkPath
#' @rdname tempfiles
td <- function(dir = tempdir()) {
  checkPath(dir, create = TRUE)
}

#' @export
#' @rdname tempfiles
tf <- function(ext = ".tif", dir = td()) {
  x <- tempfile(tmpdir = dir, fileext = ext)
  file.create(x)
  normalizePath(x, winslash = "/", mustWork = NA)
}
