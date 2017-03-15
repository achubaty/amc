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
#' @export
#' @docType methods
#' @rdname loadPackages
#' @importFrom utils install.packages
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

#' @rdname loadPackages
setMethod("loadPackages",
          signature = "character",
          definition = function(packageList, install, quiet) {
            if (install) {
              repos <- getOption("repos")
              if ( is.null(repos) || any(repos == "") ) {
                repos <- "https://cran.rstudio.com"
              }
              installed <- unname(installed.packages()[,"Package"])
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
#' @rdname detach
#' @seealso \code{\link{detach}}
#'
.detach <- function(package) {
  pkg <- deparse(substitute(package))
  pkg <- paste(unlist(strsplit(pkg, "\"")), collapse = "")
  expr <- paste0("detach(package:", pkg, ", unload=TRUE)")
  tryCatch(eval(parse(text = expr)), error = function(c) {
    c$message <- paste0("Package ", pkg, " is not attached.\n")
    message(c)
  })
}
