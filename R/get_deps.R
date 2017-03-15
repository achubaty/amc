#' Get package dependencies (offline)
#'
#' Read a package's dependencies from file, rather than searching CRAN.
#' From \url{http://stackoverflow.com/a/30225680/1380598}.
#'
#' @param path  A local file path to a package directory.
#'
#' @return A list of package dependencies.
#'
#' @author Josh O'Brien
#' @docType methods
#' @export
#' @rdname get_deps
#'
#' @examples
#' get_deps(system.file(package = "amc"))
#'
get_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  val[val != "R"]
}
