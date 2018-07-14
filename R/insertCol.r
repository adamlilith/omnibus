#' Insert a column into a data frame or matrix
#'
#' This function inserts one or more columns before or after another column in a data frame or matrix. It is similar to \code{\link[base]{cbind}} except that the inserted column(s) can be placed anywhere.
#' @param x Data frame, matrix, or vector with same number of rows or elements as \code{into}.
#' @param into Data frame or matrix into which \code{x} is to be inserted.
#' @param at Character or integer, name of column or column number at which to do insertion.
#' @param before Logical, if \code{TRUE} (default) then the insertion will occur in front of the column named in \code{at}, if \code{FALSE} then after.
#' @return A data frame.
#' @seealso \code{\link[base]{merge}}, \code{\link[base]{cbind}}
#' @examples
#' x <- data.frame(y1=11:15, y2=rev(letters)[1:5])
#' into <- data.frame(x1=1:5, x2='valid', x3=letters[1:5], x4=LETTERS[1:5], x5='stuff')
#'
#' insertCol(x, into=into, at='x3')
#' insertCol(x, into=into, at='x3', before=FALSE)
#' @export
insertCol <- function(
	x,
	into,
	at,
	before=TRUE
) {

	if (!(class(x) %in% c('data.frame', 'matrix'))) x <- as.data.frame(x)
	if (!(class(at) %in% c('numeric', 'integer'))) at <- which(names(into) %in% at)
	
	if (nrow(x) != nrow(into)) stop('Inserted column must have same number of rows as target data frame/matrix.')
	
	into <- if (at == 1) {
		cbind(x, into)
	} else if (at == ncol(into)) {
		cbind(into, x)
	} else if (before) {
		cbind(into[ , 1:(at - 1)], x, into[ , at:ncol(into)])
	} else if (!before) {
		cbind(into[ , 1:at], x, into[ , (at + 1):ncol(into)])
	}
	
	into

}
