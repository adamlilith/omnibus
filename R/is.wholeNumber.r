#' Test if a numeric value is a whole number
#'
#' @description Sometimes numeric values can appear to be whole numbers but are actually represented in the computer as floating-point values. In these cases, simple inspection of a value will not tell you if it is a whole number or not. This function tests if a number is "close enough" to an integer to be a whole number. Note that \code{\link{is.integer}} will indicate if a value is of \emph{class} integer (which if it is, will always be a whole number), but objects of class \code{numeric} will not evaluate to \code{TRUE} even if they are "supposed" to represent integers.
#'
#' @param x A numeric or integer vector.
#'
#' @param tol Largest absolute difference between a value and its integer representation for it to be considered a whole number.
#'
#' @returns A logical vector.
#'
#' @seealso \code{\link{is.integer}}
#'
#' @examples
#' 
#' x <- c(4, 12 / 3, 21, 21.1)
#' is.wholeNumber(x)
#'
#' @export
is.wholeNumber <- compiler::cmpfun(function(x, tol = .Machine$double.eps^0.5) { abs(x - round(x)) < tol })
