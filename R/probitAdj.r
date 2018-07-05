#' The inverse of the \code{\link[omnibus]{logitAdj}} function which is robust to cases that equal 0 or 1
#'
#' This function is the inverse of \code{\link[omnibus]{logitAdj}}. That function calculates the logit of values but is robust to cases where the operanc is 0 or 1. The adjusted probit is equal to \code{(base^x + epsilon * base^x - epsilon) / (base^x + 1)}.
#'
#' @param x Numeric list.
#' @param epsilon Value or character.  If a numeric value (typically ~0.01 or smaller), then this is added/subtracted from \code{x} to ensure log of 0 or 1 is not taken. If equal to \code{auto} then the value of \code{epsilon} is taken from the attributes of \code{x}. If \code{x} has no such attribute, a warning is given and a value of 0.01 is used.
#' @param base Base of logarithm. Use \code{base=exp(1)} for base \code{e}.
#' @param auto If \code{TRUE} then if the attributes of \code{x} have slots named \code{epsilon} and \code{base} then use these instead of the user-supplied values of \code{epsilon} and \code{base}. If they do not appear as attributes of \code{x} but \code{auto} is \code{TRUE} then the function prints warnings and uses 0.01 and 10, respectively. If \code{FALSE} (default) then use the user-supplied values of \code{epsilon} and \code{base}.
#' @return Numeric.
#' @seealso \code{\link[omnibus]{logitAdj}}
#' @examples
#' x <- seq(0, 1, by=0.1)
#' y <- logitAdj(x)
#' xx <- probitAdj(y, auto = TRUE)
#' @export

probitAdj <- function(
    x,
    epsilon = 0.01,
    base = 10,
    auto = FALSE
) {

	if (auto) {

		if ('epsilon' %in% names(attributes(x))) {
			epsilon <- attr(x, 'epsilon', exact = TRUE)
		} else {
			warning('Argument "epsilon" in the function probtAdj() is set to "auto" but the argument "x" has no attribute named "epsilon". Using 0.01 instead.')
			epsilon <- 0.01
		}

		if ('base' %in% names(attributes(x))) {
			base <- attr(x, 'base', exact = TRUE)
		} else {
			warning('Argument "base" in the function probtAdj() is set to "auto" but the argument "x" has no attribute named "base". Using 10 instead.')
			base <- 10
		}

	}

	x <- (base^x + epsilon * base^x - epsilon) / (base^x + 1)
	x

}
