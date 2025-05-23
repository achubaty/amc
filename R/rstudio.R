#' Check whether R is running in an Rstudio session
#'
#' Based on <https://stackoverflow.com/q/12389158/1380598>.
#'
#' @export
isRstudio <- function() {
  Sys.getenv("RSTUDIO") == 1 ||
    .Platform$GUI == "RStudio" ||
    if (suppressWarnings(requireNamespace("rstudioapi", quietly = TRUE))) {
      rstudioapi::isAvailable()
    } else {
      FALSE
    }
}
