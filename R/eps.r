#' The smallest machine-readable number
#'
#' This function returns the smallest machine-readable number (equal to \code{.Machine$double.eps}).
#' @return Numeric value.
#' @examples
#' eps()
#' @export

eps <- function() .Machine$double.eps
