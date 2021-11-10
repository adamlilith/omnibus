#' Identify values bracketing another value
#'
#' This function finds the set of values in a vector that bracket a third value, \code{x}. If \code{x} is exactly equal to one of the values in the vector, then a single value equal to \code{x} is returned. If \code{x} falls outside of the range of the vector, then the least/most extreme value of the vector is returned (depending on which side of the distribution of the vector \code{x} resides). Optionally, users can have the function return the index of the values that bracket \code{x}.
#' @param x Numeric value or vector of values.
#' @param by A vector of numeric values. These should be sorted (from high to low or low to high... if not, an error will result).
#' @param index Logical. If \code{FALSE} (default), then numeric values in \code{by} will be returned. If \code{TRUE}, then the index or indices of the bracketing value(s) will be returned.
#' @param inner Logical. If \code{TRUE} (default), then if \code{x} is surrounded by at least one series of repeating values, return the values (or indices) among the repeated sequence(s) closest to the value of \code{x}. If \code{FALSE}, return the value(s) (or indices) among the repeated sequence(s) farthest from the value of \code{x}. For example, if \code{index = TRUE}, \code{by = c(1, 2, 2, 2, 3, 3)}, and \code{x = 2.5}, setting \code{inner = TRUE} will return the index of the third 2 and first 3.  If \code{inner = FALSE}, then the function returns the index of the first 2 and second 3.
#' @param warn Logical. If \code{TRUE} (default), then warn if \code{x} is outside the range of \code{by}.
#' @return If \code{x} is a single value, then the function will return a numeric vector of length 1 or 2, depending on how many values bracket \code{x}. If all values of \code{by} are the same, then the median index (or value) of \code{by} is returned. If \code{x} is a vector, then the result will be a list with one element per item in \code{x} with each element having the same format as the case when \code{x} is a single value.
#' @examples
#' by <- 2 * (1:5)
#' bracket(4.2, by)
#' bracket(6.8, by)
#' 
#' bracket(3.2, by, index=TRUE)
#' bracket(c(3.2, 9.8, 4), by)
#'
#' bracket(2, c(0, 1, 1, 1, 3, 5), index=TRUE)
#' bracket(3, c(1, 2, 10))
#'
#' bracket(2.5, c(1, 2, 2, 2, 3, 3), index=TRUE)
#' bracket(2.5, c(1, 2, 2, 2, 3, 3), index=TRUE, inner=FALSE)
#' bracket(2.9, c(1, 2, 2, 2, 3, 3), index=TRUE)
#' bracket(2.9, c(1, 2, 2, 2, 3, 3), index=TRUE, inner=FALSE)
#' 
#' \donttest{
#' by <- 1:10
#' bracket(-100, by)
#' bracket(100, by)
#' }
#' 
#' @export

bracket <- function(x, by, index=FALSE, inner=TRUE, warn=TRUE) {

	# is "by" in order?
	bySeq <- seq_along(by)
	if (!(all(order(by) == bySeq) | all(order(by, decreasing=FALSE) == rev(bySeq)))) {
		stop('The argument "by" must be sorted in order (increasing or decreasing).')
	} else if (length(x) > 1L) {
	
		out <- list()
		for (i in seq_along(x)) {
			out[[length(out) + 1]] <- bracket(x[i], by=by, index=index, inner=inner, warn=warn)
		}
		
	} else {
	
		# exact match
		if (any(x %==na% by)) {
			firstIndex <- which(x %==na% by)
			if (!index) return(x)
			firstIndex <- round(stats::median(firstIndex))
			secondIndex <- NULL
		# x is > all values
		} else if (all(x %>na% by)) {
			firstIndex <- which.max(by)
			secondIndex <- NULL
			if (warn) warning('The value of "x" exceeds the maximum value in "by".')
		# x is < all values
		} else if (all(x %<na% by)) {
			firstIndex <- which.min(by)
			secondIndex <- NULL
			if (warn) warning('The value of "x" deceeds the minimum value in "by".')
		# x is properly bracketed
		} else {

			diffs <- abs(x - by)
			minDiff <- min(diffs)
			byMinDiff <- by[which(diffs == minDiff)]
			byMinDiff <- unique(byMinDiff)

			# if x is equidistant from at least two values
			if (length(byMinDiff) > 1) {
				
				indexFirstMinDiffs <- which(by == byMinDiff[1])
				indexSecondMinDiffs <- which(by == byMinDiff[2])
				
				if (inner) {
					firstIndex <- indexFirstMinDiffs[length(indexFirstMinDiffs)]
					secondIndex <- indexSecondMinDiffs[1]
				} else if (!inner) {
					firstIndex <- indexFirstMinDiffs[1]
					secondIndex <- indexSecondMinDiffs[length(indexSecondMinDiffs)]
				}
			
			} else {
					
				firstIndex <- which(by == byMinDiff)

				byNa <- by
				byNa[firstIndex] <- NA
				
				if (all(by[firstIndex] < x)) {
					byNa[1:firstIndex[length(firstIndex)]] <- NA
				} else if (all(by[firstIndex] > x)) {
					byNa[firstIndex[1]:length(byNa)] <- NA
				}
				
				diffs <- abs(byNa - x)
				minDiff <- min(diffs, na.rm=TRUE)
				secondIndex <- which(diffs == minDiff)
				
				l1 <- length(firstIndex)
				l2 <- length(secondIndex)
				
				if (inner) {
					if (firstIndex[1] > secondIndex[l2]) {
						firstIndex <- firstIndex[1]
						secondIndex <- secondIndex[l2]
					} else if (firstIndex[l1] < secondIndex[1]) {
						firstIndex <- firstIndex[l1]
						secondIndex <- secondIndex[1]
					}
				} else if (!inner) {
					if (firstIndex[1] > secondIndex[l2]) {
						firstIndex <- firstIndex[l1]
						secondIndex <- secondIndex[1]
					} else if (firstIndex[l1] < secondIndex[1]) {
						firstIndex <- firstIndex[1]
						secondIndex <- secondIndex[l2]
					}
				}
						
				
			} # if x is equidistant from two values
				
		} # x is properly bracketed
	
		out <- if (index) {
			c(firstIndex, secondIndex)
		} else {
			by[c(firstIndex, secondIndex)]
		}
		
		out <- sort(out)
		
	}
	
	out

}
