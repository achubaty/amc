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
#' @docType methods
#' @export
#' @importFrom magrittr '%>%'
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

#' Load packages
#'
#' Load and optionally install additional packages.
#'
#' @param packageList A list of character strings specifying
#' the names of packages to be loaded.
#'
#' @param install Logical flag. If required packages are not
#' already installed, should they be installed?
#'
#' @param quiet Logical flag. Should the final "packages loaded"
#' message be suppressed?
#'
#' @return Specified packages are loaded and attached using \code{require()},
#'         invisibly returning a logical vector of successes.
#'
#' @seealso \code{\link{require}}.
#'
#' @docType methods
#' @importFrom utils install.packages
#' @export
#' @rdname loadPackages
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   pkgs <- list("ggplot2", "lme4")
#'   loadPackages(pkgs) # loads packages if installed
#'   loadPackages(pkgs, install = TRUE) # loads packages after installation (if needed)
#' }
#'
setGeneric("loadPackages", function(packageList, install = FALSE, quiet = TRUE) {
  standardGeneric("loadPackages")
})

#' @export
#' @rdname loadPackages
setMethod("loadPackages",
          signature = "character",
          definition = function(packageList, install, quiet) {
            if (install) {
              repos <- getOption("repos")
              if (is.null(repos) || any(repos == "")) {
                repos <- "https://cran.rstudio.com"
              }
              installed <- unname(installed.packages()[, "Package"])
              toInstall <- packageList[packageList %in% installed]
              install.packages(toInstall, repos = repos)
            }

            loaded <- sapply(packageList, require, character.only = TRUE)

            if (!quiet) {
              message(paste("Loaded", length(which(loaded == TRUE)), "of",
                            length(packageList), "packages.", sep = " "))
            }
            return(invisible(loaded))
})

#' @export
#' @rdname loadPackages
setMethod("loadPackages",
          signature = "list",
          definition = function(packageList, install, quiet) {
            loadPackages(unlist(packageList), install, quiet)
})

#' Detach and unload a package
#'
#' A simple wrapper to \code{detach} using \code{unload = TRUE}.
#'
#' @param package  The name of a currently attached package.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @rdname detachPackage
#' @seealso \code{\link{detach}}
#'
detachPackage <- function(package) {
  pkg <- deparse(substitute(package))
  pkg <- paste(unlist(strsplit(pkg, "\"")), collapse = "")
  expr <- paste0("detach(package:", pkg, ", unload = TRUE)")
  tryCatch(eval(parse(text = expr)), error = function(c) {
    c$message <- paste0("Package ", pkg, " is not attached.\n")
    message(c)
  })
}
