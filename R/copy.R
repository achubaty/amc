#' Copy folders with links
#'
#' Copies folders like \code{file.copy} except it replicates links correctly on
#' unix-like systems. Based on \url{http://stackoverflow.com/a/30107868/1380598}.
#'
#' @param from  \code{character} indicating the path to the directory to be copied.
#' @param to    \code{character} indicating the path to which the directory will be copied.
#'
#' @return Logical indicating success or failure.
#'
#' @author Zach Foster
#' @author Alex Chubaty
#'
#' @docType methods
#' @export
#' @rdname dir_copy
#'
dir.copy <- function(from, to) { # nolint
  target <- file.path(to, basename(from))
  if (dir.exists(target)) stop(paste0("Target folder ", target, " already exists."))

  # Get list of all files/folders to copy
  path <- data.frame(
    target = list.files(from, recursive = TRUE, all.files = TRUE, include.dirs = TRUE),
    stringsAsFactors = FALSE
  )
  path$from <- file.path(from, path$target)
  path$to <- file.path(to, basename(from), path$target)

  # Get type of file/folders
  path$type <- factor("file", levels = c("file", "folder", "link"))
  path$type[file.info(path$from)$isdir] <- "folder"
  path$type[Sys.readlink(path$from) != ""] <- "link"

  # Remove all files that are descendants of links
  isChild <- function(query, refs) {
    sapply(refs, function(x) grepl(paste0("^", x), query) & query != x)
  }
  path <- path[!sapply(path$from, function(x) any(isChild(x, path$from) & path$type == "link")), ]

  # Make copy
  res1 <- invisible(lapply(path$to[path$type == "folder"], dir.create, recursive = TRUE))
  res2 <- invisible(file.copy(from = path$from[path$type == "file"],
                              to = path$to[path$type == "file"]))
  res3 <- invisible(file.symlink(Sys.readlink(path$from[path$type == "link"]),
                                 path$to[path$type == "link"]))

  return(all(res1, res2, res3))
}
