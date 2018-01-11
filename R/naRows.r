#' Index any rows in a data frame or matrix that contain at least one \code{NA}
#'
#' This function returns the row number of any row in a data frame or matrix that has at least one \code{NA}.
#' @param x Data frame or matrix.
#' @return Integer vector.
#' @examples
#' x <- data.frame(a=1:5, b=c(1, 2, NA, 4, 5), c=c('a', 'b', 'c', 'd', NA))
#' naRows(x)
#' @export

naRows <- function(x) {

	# x			data frame

	if (class(x) != 'data.frame') x <- as.data.frame(x)
	
	isNa <- lapply(x, is.na)
	isNa <- unlist(lapply(isNa, which))
	
	if (length(isNa) > 0) isNa <- sort(unique(isNa))
	isNa
	
}
