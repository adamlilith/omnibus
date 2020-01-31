#' Compare numeric values using <, <=, >, >=, !=, and == robustly to NAs
#'
#' This function performs simple (vectorized) comparisons using \code{<}, \code{<=}, \code{>}, \code{>=}, \code{!=}, or \code{==} between values and \emph{always} returns \code{TRUE} or \code{FALSE}. \code{TRUE} only occurs if the condition can be evaluated and it is \code{TRUE}. \code{FALSE} is returned if the condition is \code{FALSE} \emph{or} it cannot be evaluated.
#' @param op Character, the operation to perform: \code{'<'}, \code{'<='}, \code{'>'}, \code{'>='}, \code{'!='}, or \code{'=='}. Note this must be a character (i.e., put it in quotes).
#' @param x Vector of numeric, \code{NA}, and/or \code{NaN} values. This is the first value in the operation \code{x XXX y} where \code{XXX} is the operator in \code{op}. If \code{x} is shorter than \code{y} then \code{x} is recycled.
#' param y Vector of numeric, \code{NA}, and/or \code{NaN} values. This is the second value in the operation \code{x XXX y} where \code{XXX} is the operator in \code{op}. If \code{y} is shorter than \code{x} then \code{y} is recycled.
#' @return Vector of logical values.
#' @examples
#' naCompare('<', c(1, 2, NA), c(10, 1, 0))
#' naCompare('<', c(1, 2, NA), 10)
#' naCompare('<', c(1, 2, NA), NA)
#' # compare to:
#' NA < 5
#' NA < NA
#' @export
naCompare <- function(op, x, y) {

	out <- do.call(op, list(x, y))
	if (anyNA(out)) out[is.na(out)] <- FALSE
	out

}

