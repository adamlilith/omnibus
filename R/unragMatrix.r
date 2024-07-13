#' Turn a "ragged" matrix into a "ragged" vector
#'
#' This function turns a "ragged" matrix into a vector. Consider a case where you have a matrix that looks like:
#' \cr\cr
#' 1, 0,  1\cr
#' 2, 3,  NA\cr
#' NA, 4, NA\cr
#' \cr
#' Here, each row represents a series of values, where missing values are represented by \code{NA}. This can be turned into a vector form going from left to right and top to bottom of the matrix, as in \code{c(1, 0, 1, 2, 3, 4)}, plus a vector \code{c(1, 4, 6)}, which provides the index of the first non-\code{NA} value in each row of the matrix in the vector, plus another vector, \code{c(1, 1, 1, 2, 2, 3)}, indicating the row to which each value in the vector belonged.
#'
#' @param x A matrix.
#' @param skip \code{NA} (default), \code{NULL}, or a numeric, integer, or character value. Value to not include in the output. If \code{NULL}, then \emph{no} values will be skipped.
#'
#' @returns A list with one vector per matrix, plus 1) a vector named \code{startIndex} with indices of start values, and 2) a vector named \code{row} with one value per non-\code{skip} value in each matrix.
#'
#' @examples
#' 
#' # default
#' x <- matrix(c(1, 0, 1, 2, 3, NA, NA, 4, NA), byrow = TRUE, nrow = 3)
#' unragMatrix(x)
#' 
#' # skip nothing
#' unragMatrix(x, skip = NULL)
#' 
#' # skips rows with all "skip" values
#' y <- matrix(c(1, 0, 1, NA, NA, NA, NA, 4, NA), byrow = TRUE, nrow = 3)
#' unragMatrix(y)
#' 
#' @export
unragMatrix <- function(x, skip = NA) {

	out <- x[1, ]
	if (!is.null(skip)) {
		if (is.na(skip)) {
			out <- out[!is.na(out)]
		} else {
			out <- out[out != skip]
		}
	}
	
	if (length(out) > 0) {
		startIndex <- 1
		row <- rep(1, length(out))
	} else {
		startIndex <- row <- numeric()
	}
	
	if (nrow(x) > 1) {
		for (countRow in 2:nrow(x)) {
		
			thisOut <- unragMatrix(x[countRow, , drop = FALSE], skip = skip)
			out <- c(out, thisOut$x)
			startIndex <- c(startIndex, startIndex[length(startIndex)] + length(thisOut$x) + ifelse(is.null(skip), 0, 1))
			row <- c(row, rep(countRow, length(thisOut$x)))

		}
	}

	list(x = out, startIndex = startIndex, row = row)

}
