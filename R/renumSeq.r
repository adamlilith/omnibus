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
#' y <- c(1, 1, 3, 1, 8, NA, 8, 8)
#' renumSeq(y)
#'
#' z <- c('c', 'c', 'b', 'a', 'w', 'a')
#' renumSeq(z)
#' 
#' @export
renumSeq <- function(x) {

	x <- x[!is.na(x)]

	uniq <- unique(x)
	nuniq <- length(uniq)
	
	out <- factor(x, labels = seq_len(nuniq))
	out <- as.integer(out)
	out

}
