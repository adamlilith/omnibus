#' Rescale values to the range [0, 1]
#'
#' This function rescales a vector of numeric values to the range [0, 1] using \code{(x - min(x)) / (max(x - min(x)))}. Optionally and after the stretch, values = 0 can be "nudged" slightly higher to half the minimum value across the rescaled vector of values > 0.
#' @param x Numeric list.
#' @param nudge Logical, if \code{FALSE} (default) then do nothing. If \code{TRUE} then *after* rescaling to [0, 1], a small value will be added to all values of \code{x} equal to 0. This value is equal to \code{0.5 * min(x[x > 0])}.
#' @param na.rm Logical, if \code{FALSE} (default) then if any values of \code{x} are \code{NA} then the returned value will be \code{NA}. If \code{TRUE} then \code{NA}'s are ignored in calculation.
#' @return Numeric value.
#' @seealso \code{\link[base]{scale}}
#' @examples
#' x <- 1:20
#' stretchMinMax(x)
#' stretchMinMax(x, nudge=TRUE)
#' x <- c(1:20, NA)
#' stretchMinMax(x)
#' stretchMinMax(x, na.rm=TRUE)
#' stretchMinMax(x, nudge=TRUE, na.rm=TRUE)
#' @export

stretchMinMax <- function(x, nudge=FALSE, na.rm=FALSE) {

	out <- x - min(x, na.rm=na.rm)
	out <- out / max(out, na.rm=na.rm)
	if (nudge) out[out == 0] <- 0.5 * min(out[out > 0], na.rm=TRUE)
	out
	
}
