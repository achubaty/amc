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
#'                      \code{TRUE} means to use
#'                      \code{c("Depends", "Imports", "LinkingTo", "Suggests")}.
#'
#' @return A character vector of package dependencies.
#'
#' @author Josh O'Brien
#' @author Alex Chubaty
#' @export
#' @importFrom magrittr %>%
#' @rdname get_deps
#'
#' @examples
#' get_deps(system.file(package = "amc"))
#' get_deps(system.file(package = "amc"), TRUE)
#'
get_deps <- function(path, dependencies = NA) { # nolint
  allTypes <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
  if (any(is.na(dependencies))) {
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
  val <- val[val != "R"]
  if (length(val) == 0L) val <- character(0)
  return(sort(val))
}

#' Detach and unload a package
#'
#' A simple wrapper to \code{detach} using \code{unload = TRUE}.
#'
#' @param package  The name of a currently attached package.
#'
#' @author Alex Chubaty
#' @export
#' @rdname detachPackage
#' @seealso \code{\link{detach}}, \code{\link{detachAllPackages}}
detachPackage <- function(package) {
  pkg <- deparse(substitute(package))
  pkg <- paste(unlist(strsplit(pkg, "\"")), collapse = "")
  expr <- paste0("detach(package:", pkg, ", unload = TRUE)")
  tryCatch(eval(parse(text = expr)), error = function(c) {
    c$message <- paste0("Package ", pkg, " is not attached.\n")
    message(c)
  })
}

#' Forcibly detach all packages
#'
#' Based on \url{https://stackoverflow.com/a/39235076/1380598}.
#'
#' @author mmfrgmpds
#' @importFrom utils sessionInfo
#' @rdname detachAllPackages
#' @seealso \code{\link{detach}}, \code{\link{detachPackage}}
detachAllPackages <- function() {
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                   detach, character.only = TRUE, unload = TRUE, force = TRUE))
}
