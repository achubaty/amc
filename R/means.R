#' Geometric and harmonic mean
#'
#' Description needed.
#'
#' @note these have not been thoroughly tested to handle \code{NA} values, etc.
#'
#' @param x  A numeric vector.
#'
#' @param ... Additional arguments to \code{prod} or \code{mean}.
#'
#' @return A numeric vector of length one.
#'
#' @author Alex Chubaty
#' @export
#' @rdname means
#'
#' @examples
#' series <- 1:10
#' mean(series)
#' geometricMean(series)
#' harmonicMean(series)
#'
geometricMean <- function(x, ...) {
    gm <- prod(x, ...) ^ (1 / length(x))
    return(gm)
}

#' @export
#' @rdname means
harmonicMean <- function(x, ...) {
    if (length(x[x == 0])) stop("Error: contains zero values.")
    hm <- 1 / mean(1 / x, ...)
    return(hm)
}
