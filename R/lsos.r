#' Size of objects taking most memory
#'
#' Displays the largest objects in memory.
#' @param n Positive integer, maximum number of objects to display.
#' @param orderBy Either \code{'size'} (default) or \code{'name'}.
#' @param decreasing Logical, if \code{TRUE} (default), objects are displayed from largest to smallest.
#' @param pos Environment from which to obtain size of objects. Default is 1. See \code{\link{ls}}.#"
#' @param display If \code{TRUE} (default), print a table with memory used.
#' @param ... Other arguments to pass to \code{\link{ls}}.
#' @return Data frame (invisible).
#' @examples
#' lsos()
#' lsos(3)
#' @export

lsos <- function(n=10, orderBy='size', decreasing=TRUE, pos=1, display = TRUE, ...) {

	out <- sapply(ls(pos=pos, ...), function(x){ utils::object.size(get(x)) })
	if (length(out) == 0) {
		out <- NA
	} else {
		if (orderBy == 'size') {
			out <- sort(out, decreasing=decreasing)
		} else if (orderBy == 'name') {
			out <- out[order(names(out), decreasing=!decreasing)]
		}
	
		out <- out[1:min(length(out), n)]
		out <- as.data.frame(out)
		colnames(out) <- 'size'
	}
	if (show) print(out)
	invisible(out)

}
