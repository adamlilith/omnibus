#' The inverse of the [logitAdj()] function which is robust to cases that equal 0 or 1
#'
#' This function is the inverse of the \code{logitAdj()}. That function calcualtes the logit of values but is robust to cases where the operadn is 0 or 1. The adjusted probit is equal to \code{(base^x + epsilon * base^x - epsilon) / (base^x + 1)}.
#'
#' @param x Numeric list.
#' @param epsilon Value to add/subtract from x to ensure log of 0 or 1 is not taken (usually a small number).
#' @param base Base of logarithm. Use \code{base=exp(1)} for base \code{e}.
#' @return Numeric.
#' @seealso [logitAdj()]
#' @examples
#' set.seed(123)
#' x <- seq(0, 1, by=0.01)
#' logitAdj(x)
#' logitAdj(x, 0.001)
#' logitAdj(x, base=2)
#' @export

### adjusted probit
probitAdj <- function(x, epsilon = 0.01, base = 10) (base^x + epsilon * base^x - epsilon) / (base^x + 1)