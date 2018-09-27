#' Create a link to a file
#'
#' Creates a symbolic link (symlink) to a file if possible, possibly falling back to a hard link.
#' Hard links are for files only, and won't work across different physical drives.
#' Symlinks won't work on Windows without admin privileges.
#'
#' @note Use caution with files-backed objects (e.g., rasters). See examples.
#'
#' @param from,to  character vectors, containing file names or paths (can alternatively
#'                 be the path to a single existing directory).
#' @param symlink  Logical indicating whether to use symlink (instead of hardlink).
#'                 Default \code{TRUE}.
#'
#' @seealso \code{\link{file.link}}, \code{\link{file.symlink}}, \code{\link{Sys.readlink}}
#'
#' @author Alex Chubaty
#' @export
#'
#' @examples
#' library(datasets)
#' library(magrittr)
#' library(raster)
#'
#' tmpDir <- file.path(tempdir(), 'symlink-test') %>%
#'   normalizePath(winslash = '/', mustWork = FALSE)
#' dir.create(tmpDir)
#'
#' f0 <- file.path(tmpDir, "file0.csv")
#' write.csv(iris, f0)
#'
#' d1 <- file.path(tmpDir, "dir1")
#' dir.create(d1)
#' write.csv(iris, file.path(d1, "file1.csv"))
#'
#' d2 <- file.path(tmpDir, "dir2")
#' f2 <- file.path(tmpDir, "file2.csv")
#'
#' ## create a link to the the directory; d2 should look like d1
#' flink(d1, d2)  ## symlink
#' dir.exists(d2) ## TRUE
#' identical(d1, Sys.readlink(d2)) ## TRUE
#' file.exists(file.path(d2, "file1.csv")) ## TRUE
#'
#' ## create link to a file
#' flink(f0, f2) ## symlink
#' file.exists(f2) ## TRUE
#' identical(read.csv(f0), read.csv(f2)) ## TRUE
#'
#' ## deleting the link shouldn't delete the original file
#' unlink(d2, recursive = TRUE)
#' file.exists(file.path(d2, "file1.csv")) ## FALSE
#' file.exists(file.path(d1, "file1.csv")) ## TRUE
#'
#' unlink(f2)
#' file.exists(f2) ## FALSE
#' file.exists(f0) ## TRUE
#'
#' ## using rasters and other file-backed objects
#' f3 <- system.file("external/test.grd", package = "raster")
#' r3 <- raster(f3)
#' f4 <- file.path(tmpDir, "raster4.grd")
#' flink(f3, f4, FALSE) ## hardlink
#'
#' file.exists(f4) ## TRUE
#' r4 <- try(raster(f4)) ## hardlink fails
#'
#' f5 <- file.path(tmpDir, "raster5.grd")
#' flink(f3, f5, TRUE) ## SYMLINK
#' file.exists(f5) ## TRUE
#' r5 <- raster(f5) ## symlink works
#' identical(r3, r5) ## TRUE
#'
#' ## cleanup
#' unlink(tmpDir, recursive = TRUE)
#'
flink <- function(from, to, symlink = TRUE) {
  if (isTRUE(symlink)) {
    result <- suppressWarnings(file.symlink(from, to)) ## try symlink
    if (isTRUE(result)) {
      message(paste("Created SYMLINK to file", to))
    } else {
      warning("Unable to create either a symbolic link from file ", from, ".",
              if (isTRUE(file.exists(to))) paste0("\n  File ", to, " already exists."))
    }
  } else {
    result <- suppressWarnings(file.link(from, to))  ## try hardlink

    if (isTRUE(result)) {
      message(paste("Created HARDLINK to file", to))
      return(invisible(result))
    } else {
      result <- suppressWarnings(file.symlink(from, to)) ## try symlink if hardlink fails
    }
    if (isTRUE(result)) {
      message(paste("Created SYMLINK to file", to))
    } else {
      warning("Unable to create either a symbolic or hard link from file ", from, ".",
              if (isTRUE(file.exists(to))) paste0("\n  File ", to, " already exists."))
    }
  }
  return(invisible(result))
}
