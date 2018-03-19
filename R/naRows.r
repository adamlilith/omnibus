#' Index any rows in a data frame or matrix that contain at least one \code{NA}
#'
#' This function returns the row number of any row in a data frame or matrix that has at least one \code{NA}.
#' @param x Data frame or matrix.
#' @param inf Logical, if \code{TRUE} then also flag rows that contain at least one \code{Inf} or \{-Inf} value.
#' @return Integer vector.
#' @examples
#' x <- data.frame(a=1:5, b=c(1, 2, NA, 4, 5), c=c('a', 'b', 'c', 'd', NA))
#' naRows(x)
#' x <- data.frame(a=1:5, b=c(1, 2, NA, 4, 5), c=c('a', 'b', 'c', Inf, NA))
#' naRows(x, TRUE)
#' naRows(x, FALSE)
#' @export

naRows <- function(x, inf = FALSE) {

	if (class(x) != 'data.frame') x <- as.data.frame(x)
	
	focalRows <- lapply(x, is.na)
	focalRows <- unlist(lapply(focalRows, which))
	
	if (inf) {
		isInf <- lapply(x, is.infinite)
		isInf <- unlist(lapply(isInf, which))
		focalRows <- c(focalRows, isInf)
	}
	
	if (length(focalRows) > 0) focalRows <- sort(unique(focalRows))
	focalRows
	
}
