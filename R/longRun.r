#' Length of the longest run of a particular value in a numeric vector
#'
#' This function returns the lengh of the longest run of a particular numeric value in a numeric vector.  A "run" is an uninterrupted  sequence of the same number. Runs can be "wrapped" so that if the sequence starts and ends with the target value then it is considered as a consecutive run.
#' @param x Numeric vector.
#' @param val Numeric. Value of the elements of \code{x} of which to calculate length of longest run.
#' @param wrap Logical. If \code{TRUE} then runs can "wrap" from the end of \code{x} to the start of \code{x} if the first and last elements of \code{x} are equal to \code{val}.
#' @param na.rm Logical. If \code{TRUE} then remove \code{NA}s first.
#' @return Integer.
#' @seealso [base::rle()]
#' @examples
#' x <- c(1, 1, 1, 2, 2, 3, 4, 5, 6, 1, 1, 1, 1, 1)
#' longRun(x, 2)
#' longRun(x, 1)
#' longRun(x, 1, wrap=TRUE)
#' @export

longRun <- compiler::cmpfun(function(x, val, wrap = FALSE, na.rm = FALSE) {

    if (na.rm) x <- stats::na.omit(x)

	if (any(is.na(x))) {
		out <- NA
	} else if (!any(x==val)) { # no values are in run
		out <- 0
	} else if (all(x==val)) { # all values are in run
		out <- length(x)
	} else if (!wrap) { # any values in run and not wrapping
		xl <- rle(x)
		out <- max(xl$lengths[which(xl$values==val)])
	} else { # any values are in run and wrapping
		xl <- rle(x)
		if (xl$values[1]==val & xl$values[length(xl$values)]==val) {
			out <- max(
				max(xl$lengths[which(xl$values==val)]),
				xl$lengths[1] + xl$lengths[length(xl$lengths)]
			)
		} else {
			out <- max(xl$lengths[which(xl$values==val)])
		}
	}

	out

})
