#' Create a link to a file
#'
#' Creates a symbolic link (symlink) to a file if possible, falling back to a hardlink
#' if creating a symlink is not possible (usually on Windows).
#'
#' @note This has not been well tested on Windows!
#'
#' @param from,to  character vectors, containing file names or paths (can alternatively
#'                 be the path to a single existing directory).
#'
#' @seealso \code{\link{file.link}}, \code{\link{file.symlink}}, \code{\link{Sys.readlink}}
#'
#' @author Alex Chubaty
#' @export
#'
#' @examples
#' library(datasets)
#' library(magrittr)
#'
#' tmpDir <- file.path(tempdir(), 'symlink-test') %>%
#'   normalizePath(winslash = '/', mustWork = FALSE)
#' dir.create(tmpDir)
#'
#' f0 <- file.path(tmpDir, 'file0.csv')
#' write.csv(iris, f0)
#'
#' d1 <- file.path(tmpDir, 'dir1')
#' dir.create(d1)
#' write.csv(iris, file.path(d1, "file1.csv"))
#'
#' d2 <- file.path(tmpDir, 'dir2')
#' f2 <- file.path(tmpDir, 'file2.csv')
#'
#' ## create a link to the the directory; d2 should look like d1
#' flink(d1, d2)
#' dir.exists(d2) ## TRUE
#' identical(d1, Sys.readlink(d2)) ## TRUE
#' file.exists(file.path(d2, "file1.csv")) ## TRUE
#'
#' ## create link to a file
#' flink(f0, f2)
#' file.exists(f2) ## TRUE
#' identical(f0, Sys.readlink(f2)) ## TRUE
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
#' ## cleanup
#' unlink(tmpDir, recursive = TRUE)
#'
flink <- function(from, to) {
  result <- suppressWarnings(file.symlink(from, to)) ## try symlink first
  if (!isTRUE(result)) {
    result <- suppressWarnings(file.link(from, to))  ## try hardlink if symlink fails
  }

  if (!isTRUE(result)) {
    warning("Unable to create either a symbolic or hard link from file ", from, ".",
            if (isTRUE(file.exists(to))) paste0("\n  File ", to, " already exists."))
  }
  return(invisible(result))
}
