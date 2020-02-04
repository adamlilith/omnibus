#' Compare values using <, <=, >, >=, !=, and == (robust to NAs)
#'
#' This function and set of operators perform simple (vectorized) comparisons using \code{<}, \code{<=}, \code{>}, \code{>=}, \code{!=}, or \code{==} between values and \emph{always} returns \code{TRUE} or \code{FALSE}. \code{TRUE} only occurs if the condition can be evaluated and it is \code{TRUE}. \code{FALSE} is returned if the condition is \code{FALSE} \emph{or} it cannot be evaluated.
#' @param op Character, the operation to perform: \code{'<'}, \code{'<='}, \code{'>'}, \code{'>='}, \code{'!='}, or \code{'=='}. Note this must be a character (i.e., put it in quotes).
#' @param x,y Vectors of numeric, character, \code{NA}, and/or \code{NaN} values. This is the first value in the operation \code{x XXX y} where \code{XXX} is the operator in \code{op}. If \code{x} is shorter than \code{y} then \code{x} is recycled.
#' @return Vector of logical values.
#' @aliases grapes_less_than_na_grapes
#' @examples
#' naCompare('<', c(1, 2, NA), c(10, 1, 0))
#' naCompare('<', c(1, 2, NA), 10)
#' naCompare('<', c(1, 2, NA), NA)
#' # compare to:
#' NA < 5
#' NA < NA
#'
#' # same operations with operators:
#' 1 %<na% 2
#' 1 %<na% NA
#' 3 %==na% 3
#' NA %==na% 3
#' 4 %!=na% 4
#' 4 %!=na% NA
#' 5 %>=na% 3
#' 5 %>=na% NA
#' 3 %==na% c(NA, 1, 2, 3, 4)
#'
#' # compare to:
#' 1 < 2
#' 1 < NA
#' 3 == 3
#' NA == 3
#' 4 != 4
#' 4 != NA
#' 5 >= 3
#' 5 >= NA
#' 3 == c(NA, 1, 2, 3, 4)
#' @export
naCompare <- function(op, x, y) {

	out <- do.call(op, list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}

#' @describeIn naCompare Compare values using < (robust to NAs)
#' @export
`%<na%` <- function(x, y) {

	out <- do.call('<', list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}
#' @describeIn naCompare Compare values using <= (robust to NAs)
#' @export
`%<=na%` <- function(x, y) {

	out <- do.call('<=', list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}

#' @describeIn naCompare Compare values using == (robust to NAs)
#' @export
`%==na%` <- function(x, y) {

	out <- do.call('==', list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}

#' @describeIn naCompare Compare values using != (robust to NAs)
#' @export
`%!=na%` <- function(x, y) {

	out <- do.call('!=', list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}

#' @describeIn naCompare Compare values using > (robust to NAs)
#' @export
`%>na%` <- function(x, y) {

	out <- do.call('>', list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}

#' @describeIn naCompare Compare values using >= (robust to NAs)
#' @export
`%>=na%` <- function(x, y) {

	out <- do.call('>=', list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}
