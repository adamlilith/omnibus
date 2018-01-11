#' A logit() function robust to values that equal 0 or 1
#'
#' This function returns the logit value (\code{log(x / (1 - x))}) where a small value can be added to \code{x} to avoid problems of calculating the log when \code{x} equals 0 or 1.
#' @param x Numeric list.
#' @param epsilon Value to add/subtract from x to ensure log of 0 or 1 is not taken (usually a small number).
#' @param base Base of logarithm.
#' @return Numeric equal to \code{log((x + epsilon)/(1 - x + epsilon), base=base)}.
#' @seealso [probitAdj()]
#' @examples
#' set.seed(123)
#' x <- c(0, 1, runif(10))
#' logitAdj(x)
#' logitAdj(x, 0.001)
#' logitAdj(x, base=2)
#' @export

logitAdj <- function(x, epsilon = 0.01, base = 10) log((x + epsilon)/(1 - x + epsilon), base=base)