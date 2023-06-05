#' Renumber a sequence of numbers
#'
#' This function renumbers a sequence, which is helpful if "gaps" appear in the sequence. For example, consider the sequence \code{{1, 1, 3, 1, 8, 8, 8}}. This function will renumber the sequence \code{{1, 1, 2, 1, 3, 3, 3}}. \code{NA}s are ignored.
#'
#' @param x	Numerical or character vector.
#'
#' @return A vector.
#'
#' @seealso \code{\link{order}}, \code{\link{rank}}
#'
#' @examples
#' 
#' x <- c(1, 1, 3, 1, 8, 8, 8)
#' renumSeq(x)
#' 
#' x <- c(1, 1, 3, 1, 8, NA, 8, 8)
#' renumSeq(x)
#'
#' y <- c('c', 'c', 'b', 'a', 'w', 'a')
#' renumSeq(y)
#' 
#' @export
renumSeq <- function(x) {

	# x <- x - min(x, na.rm = TRUE) + 1
	xUnique <- sort(unique(x))
	n <- length(xUnique)
	if (!all(xUnique == 1:n)) {

		xNew <- rep(NA, length(x))
		for (i in seq_along(xUnique)) xNew[x == xUnique[i]] <- i
		x <- xNew
	
	}
	
	x

}
