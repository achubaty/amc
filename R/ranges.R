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
#' @importFrom methods is
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
#' Adapted from \url{https://gis.stackexchange.com/a/194838/47654}.
#' This preserves the original distribution of the data.
#'
#' @param x         A numeric vector or \code{Raster*} object.
#' @param new.range The lower and upper bounds of the new range. Default \code{c(0,1)}.
#' @param old.range (optional) The lower and upper bounds of the old range.
#'                  Default \code{c(min(x), max(x)}.
#'
#' @return A new object whose values have been rescaled.
#'
#' @author Jeffery Evans
#' @author Alex Chubaty
#' @export
#' @importFrom raster getValues maxValue minValue
#' @rdname rescale
#'
#' @examples
#' rescale(50, old.range = c(0, 100), new.range = c(0, 1)) ## 0.5
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
rescale <- function(x, new.range = c(0, 1), old.range = NULL) { # nolint
  if (is.numeric(x)) {
    if (is.null(old.range)) {
      old.range <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)) # nolint
      if (old.range[1] == old.range[2]) {
        stop("old.range must be specified for single values of x.")
      }
    }

    x.min <- old.range[1] # nolint
    x.max <- old.range[2] # nolint
    new.max <- new.range[1] # nolint
    new.min <- new.range[2] # nolint

    return(new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))) # nolint
  } else if (is(x, "Raster")) {
    if (is.null(old.range)) {
      old.range <- c(minValue(x), maxValue(x)) # nolint
      if (old.range[1] == old.range[2]) {
        stop("old.range must be specified for single values of x.")
      }
    }

    x.min <- old.range[1] # nolint
    x.max <- old.range[2] # nolint
    new.max <- new.range[1] # nolint
    new.min <- new.range[2] # nolint

    r <- x
    r[] <- new.min + (x[] - x.min) * ((new.max - new.min) / (x.max - x.min)) # nolint
    return(r)
  } else {
    stop("x must be a numeric vector or a Raster* object.")
  }
}
