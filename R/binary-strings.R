#' Convert integer to binary string
#'
#' Description needed
#'
#' @param i         Positive integer <= 2^53 (<= 9.007199e+15).
#'
#' @param maxBits  Maximum number of bits to print (default \code{NA}).
#'
#' @return Character vector.
#'
#' @author Alex Chubaty
#' @export
#' @rdname binstr
#'
#' @examples
#' x <- sample(0:9999, 10000)
#' y <- binstr(x) # length is 14 bits
#'
#' \dontrun{
#' # alternate (but slower) conversion to binary string
#' R.utils::intToBin(x)
#' }
#'
#' # convert binary string to integer value (very fast)
#' strtoi(y, base = 2)
#' strtoi(substr(y, 1, 4), base = 2)
#' strtoi(substr(y, 5, 8), base = 2)
#' strtoi(substr(y, 9, 11), base = 2)
#' strtoi(substr(y, 12, 14), base = 2)
#'
#' # see also `binary()` and `unbinary()` in the `composition` package (requires x11)
#'
binstr <- function(i, maxBits = NA) {
    if (is.na(maxBits)) maxBits <- ceiling(log2(max(i)))

    a <- 2 ^ ({maxBits - 1}:0) # nolint
    b <- 2 * a
    sapply(i, function(x) paste(as.integer((x %% b) >= a), collapse = "")) # nolint
}
