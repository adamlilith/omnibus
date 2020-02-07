#' Insert a column into a data frame or matrix
#'
#' This function inserts one or more columns before or after another column in a data frame or matrix. It is similar to \code{\link[base]{cbind}} except that the inserted column(s) can be placed anywhere.
#' @param x Data frame, matrix, or vector with same number of rows or elements as \code{into}.
#' @param into Data frame or matrix into which \code{x} is to be inserted.
#' @param at Character, integer, or \code{NULL}. Name of column or column number at which to do insertion. If \code{NULL} (default), the result is exactly the same as \code{cbind(into, x} except that it retains row numbers from \code{into}.
#' @param before Logical, if \code{TRUE} (default) then the insertion will occur in front of the column named in \code{at}, if \code{FALSE} then after. Ignored if \code{at} is \code{NULL}.
#' @return A data frame.
#' @seealso \code{\link[base]{merge}}, \code{\link[base]{cbind}}
#' @examples
#' x <- data.frame(y1=11:15, y2=rev(letters)[1:5])
#' into <- data.frame(x1=1:5, x2='valid', x3=letters[1:5], x4=LETTERS[1:5], x5='stuff')
#'
#' insertCol(x, into=into, at='x3')
#' insertCol(x, into=into, at='x3', before=FALSE)
#' insertCol(x, into)
#' @export
insertCol <- function(
	x,
	into,
	at = NULL,
	before=TRUE
) {

	if (!(class(x) %in% c('data.frame', 'matrix'))) x <- as.data.frame(x)
	if (!(class(at) %in% c('numeric', 'integer'))) at <- which(names(into) %in% at)
	
	if (nrow(x) != nrow(into)) warning('Inserted column does not have same number of rows as target data frame/matrix.')

	# x has no rows
	if (nrow(x) == 0) {
	
		into <- into
		
	# x has rows
	} else {
	
		rowNames <- row.names(into)
			
		# at is NULL
		if (is.null(at)) {
	
			into <- cbind(into, x)
	
		# x has rows and at is not NULL	
		} else {
			
			intoCols <- ncol(into)

			into <- if (at == 1 & before) {
				cbind(x, into)
			} else if (at == 1 & !before) {
				cbind(into[ , 1, drop=FALSE], x, into[ , 2:intoCols, drop=FALSE])
			} else if (at == intoCols) {
				cbind(into, x)
			} else if (before) {
				cbind(into[ , 1:(at - 1)], x, into[ , at:intoCols])
			} else if (!before) {
				cbind(into[ , 1:at], x, into[ , (at + 1):intoCols])
			}
			
		}
	  
		rownames(into) <- rowNames
		
	}
	
	into

}
