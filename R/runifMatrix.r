#' Matrix of random values, possibly with standardization by row or column
#'
#' This function creates a matrix populated by random uniform values and (at the user's discretion), standardizes the rows, columns, or entire matrix so values sum to 1.
#'
#' @param nrow,ncol Number of rows and columns.
#' @param min,max Minimum and maximum value of values.
#' @param stand Any of:
#' \itemize{
#'	\item \code{NULL} (default): No standardization.
#'	\item \code{'rows'}: Standardize so rows sum to 1.
#'	\item \code{'columns'}: Standardize so columns sum to 1.
#'	\item \code{'matrix'}: Standardize so all values sum to 1.
#' }
#' Partial matching is used and case is ignored.
#'
#' @returns A numeric matrix.
#'
#' @examples
#'
#' rows <- 4
#' cols <- 3
#'
#' runifMatrix(rows, cols)
#'
#' standByRows <- runifMatrix(rows, cols, stand = 'r')
#' standByRows
#' rowSums(standByRows)
#'
#' standByCols <- runifMatrix(rows, cols, stand = 'c')
#' standByCols
#' colSums(standByCols)
#'
#' standByMe <- runifMatrix(rows, cols, stand = 'm')
#' standByMe
#' sum(standByMe) # whenever you're in trouble
#'
#' @aliases runifMatrix
#' @rdname runifMatrix
#' @export runifMatrix
runifMatrix <- function(nrow, ncol, min = 0, max = 1, stand = NULL) {

	n <- nrow * ncol
	r <- stats::runif(n, min, max)
	x <- matrix(r, nrow = nrow, ncol = ncol)

	if (!is.null(stand)) {
	
		options <- c('rows', 'columns', 'matrix')
		stand <- pmatchSafe(stand, options, nmax = 1)

		if (stand == 'rows') {
			sums <- rowSums(x)
			x <- sweep(x, 1, sums, '/')
		} else if (stand == 'columns') {
			sums <- colSums(x)
			x <- sweep(x, 2, sums, '/')
		} else if (stand == 'matrix'){
			sums <- sum(x)
			x <- x / sums
		}

	}
	x

}
