#' Get package dependencies (offline)
#'
#' Read a package's dependencies from file, rather than searching CRAN.
#' Based on \url{http://stackoverflow.com/a/30225680/1380598}.
#'
#' @param path          A local file path to a package directory.
#' @param dependencies  Logical indicating whether to also install uninstalled
#'                      packages which these packages depend on/link to/import/suggest
#'                      (and so on recursively).
#'                      Can also be a character vector, a subset of
#'                      \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.\cr
#'                      The default, \code{NA}, means \code{c("Depends", "Imports", "LinkingTo")}.
#'                      \code{TRUE} means to use \code{c("Depends", "Imports", "LinkingTo", "Suggests")}.
#'
#' @return A character vector of package dependencies.
#'
#' @author Josh O'Brien
#' @author Alex CHubaty
#' @docType methods
#' @export
#' @importFrom magrittr '%>%'
#' @rdname get_deps
#'
#' @examples
#' get_deps(system.file(package = "amc"))
#' get_deps(system.file(package = "amc"), TRUE)
#'
get_deps <- function(path, dependencies = NA) {
  allTypes <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  if (is.na(dependencies)) {
    dependencies <-  c("Depends", "Imports", "LinkingTo")
  } else if (isTRUE(dependencies)) {
    dependencies <- c("Depends", "Imports", "LinkingTo", "Suggests")
  } else if (!all(dependencies %in% allTypes)) {
    stop("invalid dependency type.")
  }

  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(dependencies, colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE) %>%
    trimws() %>%
    gsub("\\s.*", "", .) %>%
    sapply(., strsplit, split = "\\(") %>%
    sapply(., `[`, 1) %>%
    unname()
  return(sort(val[val != "R"]))
}
