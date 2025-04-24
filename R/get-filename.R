#' Get the name of a `source`-ed file
#'
#' Use `getFileName` in a file that is `source`-ed.
#' Based on <http://stackoverflow.com/a/1816487/1380598>.
#'
#' @param fullname   Logical (default `FALSE`) indicating whether the full
#'                   path should be returned.
#'
#' @return Character string representing the filename.
#'
#' @author Alex Chubaty
#' @export
#' @rdname getFileName
setGeneric("getFileName", function(fullname) {
  standardGeneric("getFileName")
})

#' @rdname getFileName
setMethod("getFileName",
          signature = "logical",
          definition = function(fullname) {
            f <- lapply(sys.frames(), function(i) i$filename) |>
              Filter(Negate(is.null), x = _) |>
              unlist()
            if (fullname) {
              f <- normalizePath(file.path(getwd(), f), winslash = "/")
            } else {
              f <- basename(f)
            }
            return(f)
})
