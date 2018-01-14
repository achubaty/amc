#' Hill function
#'
#' @param a DESCRIPTION NEEDED
#' @param b DESCRIPTION NEEDED
#' @param z DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @author Devin Goodsman
#' @export
#'
hill <- function(a, b, z) {
  stopifnot(is.numeric(a), is.numeric(b), is.numeric(z))

  exp(a) * z^b / (1 + exp(a) * z^b)
}

#' Logit function
#'
#' @param p DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
#'
logit <- function(p) {
  stopifnot(is.numeric(p))

  log(p / (1 - p))
}
