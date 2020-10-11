#' Round to nearest target value
#'
#' This function rounds a value to a nearest "target" value (e.g., you could round 0.72 to the nearest 0.25, or 0.75).
#' @param x Numeric.
#' @param target Numeric.
#' @param roundFx Any of \code{\link{round}}, \code{\link{floor}}, or \code{\link{ceiling}}. 
#' @return Numeric.
#' @examples
#' roundTo(0.73, 0.05)
#' roundTo(0.73, 0.1)
#' roundTo(0.73, 0.25)
#' roundTo(0.73, 0.25, floor)
#' roundTo(0.73, 1)
#' roundTo(0.73, 10)
#' roundTo(0.73, 10, ceiling)
#' @export

roundTo <- compiler::cmpfun(function(x, target, roundFx=round) {

	target * roundFx(100 * x / (100 * target))

})
