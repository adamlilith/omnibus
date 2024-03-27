#' Compare values to floating-point precision
#'
#' These functions compare values while accounting for differences in floating point precision.
#'
#' @param x,y Numeric
#' @param op Operator for comparison (must be in quotes): \code{"<"}, \code{">"}, \code{"<="}, \code{">="}, \code{"=="}, or \code{"!="}
#' @param tol Tolerance value: The largest absolute difference between \code{x} and \code{y} that is to be considered equality. The default is \code{.Machine$double.eps^0.5}.
#'
#' @return \code{TRUE}, \code{FALSE}, or \code{NA}
#' 
#' @examples
#' x <- 0.9 - 0.8
#' y <- 0.8 - 0.7
#' 
#' x < y
#' x %<% y
#' compareFloat(x, y, "<")
#' 
#' x <= y
#' x %<=% y
#' compareFloat(x, y, "<=")
#' 
#' x == y
#' x %==% y
#' compareFloat(x, y, "==")
#' 
#' y > x
#' y %>% x
#' compareFloat(y, x, ">")
#' 
#' y >= x
#' y %>=% x
#' compareFloat(y, x, ">=")
#' 
#' x != y
#' x %!=% y
#' compareFloat(x, y, "!=")
#' 
#' @export
compareFloat <- compiler::cmpfun(function(x, y, op, tol = .Machine$double.eps^0.5) {

	out <- if (op == "<") {
		(y - x > tol)
	} else if (op == "<=") {
		(y - x > tol | abs(y - x) < tol)
	} else if (op == "==") {
		(abs(y - x) < tol)
	} else if (op == ">") {
		(x - y > tol)
	} else if (op == ">=") {
		(x - y > tol | abs(x - y) < tol)
	} else if (op == "!=") {
		(abs(x - y) > tol)
	} else {
		NA
	}

	out

})

#' @name `%<%`
#' @title Compare values to floating-point precision
#' @rdname compareFloat
#' @export
`%<%` <- function(x, y) do.call(compareFloat, list(x = x, y = y, op = '<'))

#' @name `%<=%`
#' @title Compare values to floating-point precision
#' @rdname compareFloat
#' @export
`%<=%` <- function(x, y) do.call(compareFloat, list(x = x, y = y, op = '<='))

#' @name `%==%`
#' @title Compare values to floating-point precision
#' @rdname compareFloat
#' @export
`%==%` <- function(x, y) do.call(compareFloat, list(x = x, y = y, op = '=='))

#' @name `%>=%`
#' @title Compare values to floating-point precision
#' @rdname compareFloat
#' @export
`%>=%` <- function(x, y) do.call(compareFloat, list(x = x, y = y, op = '>='))

#' @name `%>%`
#' @title Compare values to floating-point precision
#' @rdname compareFloat
#' @export
`%>%` <- function(x, y) do.call(compareFloat, list(x = x, y = y, op = '>'))

#' @name `%!=%`
#' @title Compare values to floating-point precision
#' @rdname compareFloat
#' @export
`%!=%` <- function(x, y) do.call(compareFloat, list(x = x, y = y, op = '!='))
