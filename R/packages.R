#' Get package dependencies (offline)
#'
#' Read a package's dependencies from file, rather than searching CRAN.
#' Based on <http://stackoverflow.com/a/30225680/1380598>.
#'
#' @param path          A local file path to a package directory.
#' @param dependencies  Logical indicating whether to also install uninstalled
#'                      packages which these packages depend on/link to/import/suggest
#'                      (and so on recursively).
#'                      Can also be a character vector, a subset of
#'                      `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.\cr
#'                      The default, `NA`, means `c("Depends", "Imports", "LinkingTo")`.
#'                      `TRUE` means to use
#'                      `c("Depends", "Imports", "LinkingTo", "Suggests")`.
#'
#' @return A character vector of package dependencies.
#'
#' @author Josh O'Brien
#' @author Alex Chubaty
#' @export
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
  val <- unlist(strsplit(dcf[, jj], ","), use.names = FALSE) |>
    trimws() |>
    gsub("\\s.*", "", x = _) |>
    sapply(strsplit, split = "\\(") |>
    sapply(`[`, 1) |>
    unname()
  val <- val[val != "R"]
  if (length(val) == 0L) val <- character(0)
  return(sort(val))
}

#' Detach and unload a package
#'
#' A simple wrapper to `detach` using `unload = TRUE`.
#'
#' @param package  The name of a currently attached package.
#'
#' @author Alex Chubaty
#' @export
#' @rdname detachPackage
#' @seealso [detach()], [detachAllPackages()]
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
#' Based on <https://stackoverflow.com/a/39235076/1380598>.
#'
#' @author mmfrgmpds
#' @importFrom utils sessionInfo
#' @rdname detachAllPackages
#' @seealso [detach()], [detachPackage()]
detachAllPackages <- function() {
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                   detach, character.only = TRUE, unload = TRUE, force = TRUE))
}

#' Determine a package's minimum R version requirement based on its dependencies
#'
#' Based on <https://stackoverflow.com/q/38686427>.
#'
#' @param package           Character string giving the name of a package whose
#'                          dependencies should be checked.
#'
#' @param exclude_main_pkg  Logical indicating whether `package` should be
#'                          excluded from the check. Default `TRUE`.
#'
#' @author hrbrmstr and Jack Wasey
#' @export
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages compareVersion contrib.url
min_r_version <- function(package = NULL, exclude_main_pkg = TRUE) {
  stopifnot(!is.null(package))

  repo <- getOption("repo", "https://cloud.r-project.org")

  avail <- utils::available.packages(utils::contrib.url(repo))
  deps <- tools::package_dependencies(package, db = avail, recursive = TRUE)
  if (is.null(deps))
    stop("package '", package, "' not found.")

  pkgs <- deps[[1]]

  matches <- avail[ , "Package"] %in% pkgs
  pkg_list <- avail[matches, "Depends"]
  vers <- grep("^R$|^R \\(.*\\)$", pkg_list, value = TRUE)
  vers <- gsub("[^0-9.]", "", vers)
  if (length(vers) == 0)
    return("Not specified")

  max_ver <- vers[1]
  if (length(vers) == 1)
    return(max_ver)

  for (v in 2:length(vers))
    if (utils::compareVersion(vers[v], max_ver) > 0)
      max_ver <- vers[v]

  max_ver
}

#' Determine source of installed packages
#'
#' Which packages were installed from CRAN, GitHub, Bioconductor, etc.?
#'
#' @inheritParams utils::packageDescription
#'
#' @export
#' @importFrom utils packageDescription
#'
#' @examples
#' pkgs <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
#' ids <- which(!(pkgs$Priority %in% c("base", "recommended")))
#' pkgs <- pkgs[ids, ]
#' pkgs <- pkgs$Package
#' pkgs[pkgSrc(pkgs) == "CRAN"]
#'
pkgSrc <- Vectorize(function(pkg, lib.loc = NULL) {
  desc <- utils::packageDescription(pkg, lib.loc)
  w <- as.character(desc$Repository)
  if (length(w) == 0) {
    x <- as.character(desc$GithubUsername)
    y <- as.character(desc$GithubRepo)
    z <- as.character(desc$GithubRef)
    if (length(y) == 0) {
      return("Other")
    } else {
      return(paste0(x, "/", y, "@", z))
    }
  } else {
    return(w)
  }
}, vectorize.args = "pkg")
