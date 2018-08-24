#' Geometric mean
#'
#' Calculate the geometric mean, with optional removal of \code{NA}'s and propagation of zeros (adapted from Paul McMurdie on StackOverflow)[https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in].)
#' @param x Numeric list.
#' @param prop0 Logical, if \code{FALSE} (default) then if any value in \code{x} equals 0 then the output will be zero. If \code{TRUE}, then zero values will be removed before calculation of the geometric mean.
#' @param na.rm Logical, if \code{TRUE} then remove \code{NA} values first.
#' @examples
#' x <- seq(0.01, 1, by=0.01)
#' mean(x)
#' geoMean(x)
#' x <- seq(0, 1, by=0.01)
#' mean(x)
#' geoMean(x)
#' geoMean(x, prop0=TRUE)
#' @export
geoMean <- function(x, prop0 = FALSE, na.rm=TRUE) {
	
	if (na.rm) x <- na.omit(x)
	out <- if (any(x < 0, na.rm = TRUE)){
		NaN
	} else if (prop0) {
		exp(mean(log(x[x > 0])))
	} else {
		exp(sum(log(x), na.rm=na.rm) / length(x))
	}
	
	out
	
}
