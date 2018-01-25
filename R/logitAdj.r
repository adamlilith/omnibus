#' A logit() function robust to values that equal 0 or 1
#'
#' This function returns the logit value (\code{log(x / (1 - x))}) where a small value can be added to \code{x} to avoid problems of calculating the log when \code{x} equals 0 or 1.
#' @param x Numeric list.
#' @param epsilon Value to add/subtract from x to ensure log of 0 or 1 is not taken (usually a small number). If \code{NULL}, then the smallest value of any \code{x} > 0 and \code{1 - x} for all \code{x} < 1 is used.
#' @param base Base of logarithm.
#' @return Numeric equal to \code{log((x + epsilon)/(1 - x + epsilon), base=base)}.
#' @seealso \code{\link[omnibus]{probitAdj}}
#' @examples
#' set.seed(123)
#' x <- seq(0, 1, by=0.01)
#' logitAdj(x)
#' logitAdj(x, 0.001)
#' probitAdj(x, 0.001)
#' probitAdj(x, 0.001)
#' probitAdj(x, auto = TRUE)
#' @export

logitAdj <- function(x, epsilon = 0.01, base = 10) {

	if (is.null(epsilon)) epsilon <- min(x[x > 0], 1 - x[x < 1], na.rm=TRUE)
	
	x <- log((x + epsilon)/(1 - x + epsilon), base=base)
	attr(x, 'epsilon') <- epsilon
	attr(x, 'base') <- base
	x
	
}
