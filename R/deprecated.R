#' Deprecated functions
#'
#' These functions have been deprecated and will be removed in a future release.
#'
#' @export
#' @inheritParams .gc
#' @rdname amc-deprecated
#'
.cleanup <- function() {
  .Deprecated(".gc", old = ".cleanup")
  .gc()
}
