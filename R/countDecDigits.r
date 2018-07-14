#' Calculate number of digits after a decimal place
#'
#' This function calculates the number of digits after a decimal place. Note that trailing zeros wil likely be ignored.
#' @param x Numeric or numeric list.
#' @return Integer.
#' @examples
#' countDecDigits(c(1, 1.1, 1.12, 1.123, 1.1234, -1, 0, 10.0000, 10.0010))
#' @export
countDecDigits <- function(x) {

	x <- as.character(abs(x))
	x <- strsplit(x, '\\.')
	x <- sapply(x, '[', 2)
	x <- nchar(x)
	if (anyNA(x)) x[is.na(x)] <- 0
	x

}
