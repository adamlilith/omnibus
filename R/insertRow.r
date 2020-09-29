#' Insert a row into a data frame or matrix
#'
#' This function inserts one or more rows before or after another row in a data frame or matrix. It is similar to \code{\link{rbind}} except that the inserted rows(s) can be placed anywhere.
#' @param x Data frame, matrix, or vector with same number of columns or elements as \code{into}.
#' @param into Data frame or matrix into which \code{x} is to be inserted.
#' @param at Character, integer, or \code{NULL}. Name of row or row number at which to do insertion. If \code{NULL} (default), the result is exactly the same as \code{rbind(into, x}.
#' @param before Logical, if \code{TRUE} (default) then the insertion will occur immediately above the row named in \code{at}, if \code{FALSE} then after. Ignored if \code{at} is \code{NULL}.
#' @return A data frame.
#' @seealso \code{\link[base]{merge}}, \code{\link{rbind}}, \code{\link{insertCol}}
#' @examples
#' x <- data.frame(x1=1:3, x2=LETTERS[1:3])
#' into <- data.frame(x1=11:15, x2='valid')
#' row.names(into) <- letters[1:5]
#'
#' insertRow(x, into=into, at='b')
#' insertRow(x, into=into, at='b', before=FALSE)
#' insertRow(x, into)
#' @export
insertRow <- function(
	x,
	into,
	at = NULL,
	before = TRUE
) {

	if (ncol(x) != ncol(into)) warning('Inserted row(s) do not have same number of columns as target data frame/matrix.')

	# if (!(class(x) %in% c('data.frame', 'matrix'))) x <- as.data.frame(x)
	if (!is.null(at) && !(class(at) %in% c('numeric', 'integer'))) at <- which(rownames(into) %in% at)
	
	# x has no rows
	if (ncol(into) == 0) {
	
		into <- x
		
	# x has rows
	} else {
	
		# at is NULL
		if (is.null(at)) {
	
			into <- rbind(into, x)
	
		# x has columns and at is not NULL	
		} else {
			
			intoRows <- nrow(into)

			into <- if (at == 1 & before) {
				rbind(x, into)
			} else if (at == 1 & !before) {
				rbind(into[1, , drop=FALSE], x, into[2:intoRows, , drop=FALSE])
			} else if (at == intoRows) {
				rbind(into, x)
			} else if (before) {
				rbind(into[1:(at - 1), , drop=FALSE], x, into[at:intoRows, , drop=FALSE])
			} else if (!before) {
				rbind(into[1:at, , drop=FALSE], x, into[(at + 1):intoRows, , drop=FALSE])
			}
			
		}
	  
	}
	
	into

}
