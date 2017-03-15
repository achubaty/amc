#' Deprecated functions
#'
#' These functions have been deprecated and will be removed in a future release.
#'
#' @export
#' @inheritParams .gc
#' @rdname grainscape-deprecated
#'
.cleanup <- function() {
  .Deprecated(".gc", old = ".cleanup")
  .gc()
}
