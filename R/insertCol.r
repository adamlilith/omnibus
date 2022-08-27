#' Insert a column or row into a data frame or matrix
#'
#' This function inserts one or more columns or rows before or after another column or row in a data frame or matrix. It is similar to \code{\link{cbind}} except that the inserted column(s)/row(s) can be placed anywhere.
#' @param x Data frame, matrix, or vector with same number of columns or rows or elements as \code{into}.
#' @param into Data frame or matrix into which \code{x} is to be inserted.
#' @param at Character, integer, or \code{NULL}. Name of column or column number or name of row or row number at which to do insertion. If \code{NULL} (default), the result is exactly the same as \code{cbind(into, x} except that it retains row numbers or column names from \code{into}.
#' @param before Logical, if \code{TRUE} (default) then the insertion will occur in front of the column or row named in \code{at}, if \code{FALSE} then after. Ignored if \code{at} is \code{NULL}.
#' @return A data frame.
#' @seealso \code{\link[base]{merge}}, \code{\link{cbind}}, \code{\link{insertRow}}
#' @examples
#'
#' x <- data.frame(y1=11:15, y2=rev(letters)[1:5])
#' into <- data.frame(x1=1:5, x2='valid', x3=letters[1:5], x4=LETTERS[1:5], x5='stuff')
#'
#' insertCol(x, into=into, at='x3')
#' insertCol(x, into=into, at='x3', before=FALSE)
#' insertCol(x, into)
#'
#' x <- data.frame(x1=1:3, x2=LETTERS[1:3])
#' into <- data.frame(x1=11:15, x2='valid')
#' row.names(into) <- letters[1:5]
#'
#' insertRow(x, into=into, at='b')
#' insertRow(x, into=into, at='b', before=FALSE)
#' insertRow(x, into)
#'
#' @export
insertCol <- function(
	x,
	into,
	at = NULL,
	before = TRUE
) {

	if (nrow(x) != nrow(into)) warning('Inserted column does not have same number of rows as target data frame/matrix.')

	if (!is.null(at) && !(inherits(at, c('numeric', 'integer')))) at <- which(colnames(into) %in% at)

	# x has no rows
	if (nrow(into) == 0) {

		into <- x

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

#' @describeIn insertCol Insert a column or row into a data frame or matrix
#' @export
insertRow <- function(
	x,
	into,
	at = NULL,
	before = TRUE
) {

	if (ncol(x) != ncol(into)) warning('Inserted row(s) do not have same number of columns as target data frame/matrix.')

	# if (!(class(x) %in% c('data.frame', 'matrix'))) x <- as.data.frame(x)
	if (!is.null(at) & !(inherits(at, c('numeric', 'integer')))) {
		at <- which(rownames(into) %in% at)
	}

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
