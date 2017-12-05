#' Get the name of a \code{source}-ed file
#'
#' Use \code{getFileName} in a file that is \code{source}-ed.
#' Based on \url{http://stackoverflow.com/a/1816487/1380598}.
#'
#' @param fullname   Logical (default \code{FALSE}) indicating whether the full
#'                   path should be returned.
#'
#' @return Character string representing the filename.
#'
#' @author Alex Chubaty
#' @docType methods
#' @export
#' @importFrom magrittr %>%
#' @rdname getFileName
#'
setGeneric("getFileName", function(fullname) {
  standardGeneric("getFileName")
})

#' @rdname getFileName
setMethod("getFileName",
          signature = "logical",
          definition = function(fullname) {
            f <- lapply(sys.frames(), function(i) i$filename) %>%
              Filter(Negate(is.null), .) %>%
              unlist
            if (fullname) {
              f <- normalizePath(file.path(getwd(), f), winslash = "/")
            } else {
              f <- basename(f)
            }
            return(f)
})
