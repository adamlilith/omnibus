#' Rescale values to the range [0, 1]
#'
#' This function rescales a vector of numeric values to an arbitrary range. Optionally, after the stretch values equal to the lowest value can be "nudged" slightly higher to half the minimum value across the rescaled vector of values > 0.
#' @param x Numeric list.
#' @param lower Numeric, low end of range to which to stretch.
#' @param upper Numeric, high end of range to which to stretch.
#' @param nudge Logical, if \code{FALSE} (default) then do nothing. If \code{TRUE} then *after* rescaling to [0, 1], a small value will be added to all values of \code{x} equal to 0. This value is equal to \code{0.5 * min(x[x > 0])}.
#' @param na.rm Logical, if \code{FALSE} (default) then if any values of \code{x} are \code{NA} then the returned value will be \code{NA}. If \code{TRUE} then \code{NA}'s are ignored in calculation.
#' @return Numeric value.
#' @seealso \code{\link[base]{scale}}
#' @examples
#' x <- 1:10
#' stretchMinMax(x)
#' stretchMinMax(x, lower=2, upper=5)
#' stretchMinMax(x, nudgeUp=TRUE)
#' stretchMinMax(x, lower=2, upper=5, nudgeUp=TRUE)
#' stretchMinMax(x, nudgeDown=TRUE)
#' stretchMinMax(x, lower=2, upper=5, nudgeUp=TRUE, nudgeDown=TRUE)
#' x <- c(1:5, NA)
#' stretchMinMax(x)
#' stretchMinMax(x, na.rm=TRUE)
#' @export

stretchMinMax <- function(x, lower=0, upper=1, nudgeUp=FALSE, nudgeDown=FALSE, na.rm=FALSE) {

	out <- x - min(x, na.rm=na.rm)
	out <- out / max(out, na.rm=na.rm)
	out <- out * (upper - lower) + lower

	if (nudgeUp) out[out == lower] <- out[out == lower] + 0.5 * (min(out[out > lower], na.rm=na.rm) - lower)
	if (nudgeDown) out[out == upper] <- out[out == upper] - 0.5 * (upper - max(out[out < upper], na.rm=na.rm))
	out
	
}
