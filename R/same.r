#' Tests if all elements of a vector are the same
#'
#' @param x Numeric or character vector.
#' @param na.rm Logical, if \code{TRUE} then remove \code{NA}s first. If \code{FALSE} and \code{NA}s are included with other values, then the function returns \code{FALSE}.
#' @examples
#' same(1:10)
#' same(rep(3, 10))
#' same(letters[1:3])
#' same(c('r', 'r', 'r'))
#' same(c('r', 'r', 'r', NA))
#' same(c('r', 'r', 'r', NA), na.rm=TRUE)
#' @export
same <- function(x, na.rm = FALSE) {

	if (na.rm) {
		if (anyNA(x)) x <- x[!is.na(x)]
		out <- all(x == unique(x))
	} else {
		if (anyNA(x) & !all(is.na(x))) {
			out <- FALSE
		} else {
			out <- all(x == omnibus::mmode(x))
		}
	}
	
	out

}
