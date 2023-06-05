#' Vectorized test for truth robust to NA
#'
#' These functions work exactly the same as \code{x == TRUE} and \code{x == FALSE} but by default return \code{FALSE} for cases that are \code{NA}.
#' @param x Logical, or a condition that evaluates to logical, or a vector of logical values or conditions to evaluate.
#' @param ifNA Logical, value to return if the result of evaluating \code{x} is \code{NA}. Note that this can be anything (i.e., \code{TRUE}, \code{FALSE}, a number, etc.).
#' @return Logical or value specified in \code{ifNA}.
#' @seealso \code{\link{isTRUE}}, \code{\link{isFALSE}}, \code{\link{TRUE}}, \code{\link{logical}}
#' @examples
#' x <- c(TRUE, TRUE, FALSE, NA)
#' x == TRUE
#' isTRUENA(x)
#' x == FALSE
#' isFALSENA(x)
#' isTRUENA(x, ifNA = Inf)
#' # note that isTRUE and isFALSE are not vectorized
#' isTRUE(x)
#' isFALSE(x)
#' @export
isTRUENA <- compiler::cmpfun(function(x, ifNA = FALSE) {
	
	out <- (x == TRUE)
	if (anyNA(x)) out[is.na(x)] <- ifNA
	out
	
})

#' @describeIn isTRUENA Vectorized test for truth robust to NA
#' @export
isFALSENA <- compiler::cmpfun(function(x, ifNA = FALSE) {
	
	out <- (x == FALSE)
	if (anyNA(x)) out[is.na(x)] <- ifNA
	out
	
})
