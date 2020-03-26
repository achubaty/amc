#' Test whether a number lies within range \code{[a,b]}
#'
#' Default values of \code{a=0; b=1} allow for quick test if
#' \code{x} is a probability.
#'
#' @param x   values to be tested
#' @param a   lower bound (default 0)
#' @param b   upper bound (default 1)
#'
#' @return Logical vectors. \code{NA} values in \code{x} are retained.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom raster getValues
#' @rdname inRange
#'
#' @examples
#' set.seed(100)
#' x <- stats::rnorm(4) ## -0.50219235  0.13153117 -0.07891709  0.88678481
#' inRange(x, 0, 1)     ## FALSE  TRUE FALSE  TRUE
#'
inRange <- function(x, a = 0, b = 1) {
  if (is.null(x)) return(NULL) # is this desired behaviour?
  if (!is.numeric(x)) {
    if (is(x, "Raster")) {
      x <- getValues(x)
    } else {
      stop("x must be numeric.")
    }
  }
  if (!is.numeric(a) || !is.numeric(b)) stop("invalid (non-numeric) bounds.")
  if (is.na(a) || is.na(b)) stop("invalid (NA) bounds.")
  if (a >= b) stop("a cannot be greater than b.")
  return((x - a) * (b - x) >= 0) # NAs will propagate -- is this desired? # nolint
}

#' Rescale values to a new range
#'
#' @param x    A numeric vector or \code{Raster*} object.
#' @param to   The lower and upper bounds of the new range. Default \code{c(0,1)}.
#' @param from (optional) The lower and upper bounds of the old range (calculated from \code{x}).
#' @param ...  Additional arguments (not used).
#'
#' @note Objects with values that are all equal (e.g., all zeroes) will be returned as-is.
#'       This behaviour differs from \code{scales:rescale} which would return a value of \code{0.5}.
#'
#' @return A new object whose values have been rescaled.
#'
#' @export
#' @rdname rescale
#'
#' @examples
#' rescale(50, from = c(0, 100), to = c(0, 1)) ## 0.5
#'
#' x <- 0:100
#' rescale(x) # defaults to new range [0,1]
#' rescale(x, c(-1, 1))
#'
#' f <- system.file("external/test.grd", package = "raster")
#' r <- raster::raster(f)
#' rescale(r) # defaults to new range [0,1]
#' rescale(r, c(-1, 1))
#'
rescale <- function(x, to, from, ...) {
  UseMethod("rescale")
}

#' @export
#' @rdname rescale
rescale.numeric <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...) {
  if (isTRUE(all.equal(from[1], from[2]))) {
    return(x)
  } else {
    xMin <- from[1]
    xMax <- from[2]
    newMin <- to[1]
    newMax <- to[2]

    return(newMin + (x - xMin) * ((newMax - newMin) / (xMax - xMin)))
  }
}

#' @export
#' @importFrom raster getValues setValues
#' @rdname rescale
rescale.RasterLayer <- function(x, to = c(0, 1),
                                from = range(getValues(x), na.rm = TRUE, finite = TRUE), ...) {
  newVals <- rescale(getValues(x), to, from, ...)
  return(setValues(x, newVals))
}
