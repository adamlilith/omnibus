#' Identify values bracketing another value
#'
#' This function finds the set of values in a vector that bracket a third value, \code{x}. If \code{x} is exactly equal to one of the values in the vector, then a single value equal to \code{x} is returned. If \code{x} falls outside of the range of the vector, then the least/most extreme value of the vector is returned (depending on which side of the distribution of the vector \code{x} resides).  Optionally, users can have the function return the index of the values that bracket \code{x}.
#' @param x Numeric value or vector of values.
#' @param by A vector of numeric values. These should be sorted (from high to low or low to high... if not, an error will result).
#' @param index Logical. If \code{FALSE} (default), then numeric values in \code{by} will be returned. If \code{TRUE}, then the index or indices of the bracketing value(s) will be returned.
#' @param warn Logical. If \code{TRUE} (default), then warn if \code{x} is outside the range of \code{by}.
#' @return If \code{x} is a single value, then the function will return a numeric vector of length 1 or 2, depending on how many values bracket \code{x}. If \code{x} is a vector, then the result will be a list with one element per item in \code{x} with each element having the same format as the case when \code{x} is a single value.
#' @examples
#' x <- 3.2
#' by <- 2 * (1:10)
#' bracket(3.2, by)
#' bracket(6.8, by)
#' bracket(18, by)
#' 
#' bracket(3.2, by, index=TRUE)
#'
#' bracket(c(3.2, 9.8, 4), by)
#'
#' bracket(2, c(0, 1, 1, 1, 3, 5), index=TRUE)
#' bracket(3, c(1, 2, 10))
#' 
#' \donttest{
#' by <- 1:10
#' bracket(-100, by)
#' bracket(100, by)
#' }
#' 
#' @export
bracket <- function(x, by, index=FALSE, warn=TRUE) {

	# is "by" in order?
	bySeq <- seq_along(by)
	if (!(all(order(by) == bySeq) | all(order(by, decreasing=FALSE) == rev(bySeq)))) {
		stop('The argument "by" must be sorted in order (increasing or decreasing).')
	} else if (length(x) > 1L) {
	
		out <- list()
		for (i in seq_along(x)) {
			out[[length(out) + 1]] <- bracket(x[i], by=by, index=index, warn=warn)
		}
		
	} else {
	
		if (any(x %==na% by)) {
			firstIndex <- which(x %==na% by)
			firstIndex <- if (tiesOuter) {
				firstIndex[1]
			} else {
				firstIndex[length(firstIndex)]
			}
			secondIndex <- NULL
		} else if (all(x %>na% by)) {
			firstIndex <- which.max(by)
			secondIndex <- NULL
			if (warn) warning('The value of "x" exceeds the maximum value in "by".')
		} else if (all(x %<na% by)) {
			firstIndex <- which.min(by)
			secondIndex <- NULL
			if (warn) warning('The value of "x" is lower than the minimum value in "by".')
		} else {

			firstIndex <- which.min(abs(x - by))

			byNa <- by
			if (byNa[firstIndex] < x) {
				byNa[1:firstIndex] <- NA
			} else if (byNa[firstIndex] > x) {
				byNa[firstIndex:length(byNa)] <- NA
			}
			
			if (any(byNa %==na% by[firstIndex])) byNa[byNa %==na% by[firstIndex]] <- NA

			secondIndex <- which.min(abs(x - byNa))
			
		}
	
		out <- if (index) {
			c(firstIndex, secondIndex)
		} else {
			by[c(firstIndex, secondIndex)]
		}
		
		out <- sort(out)
		
	}
	
	out

}
