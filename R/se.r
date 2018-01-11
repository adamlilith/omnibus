#' Standard error
#'
#' Calculate the standard error of the mean.
#' @param x	Numeric list.
#' @param na.rm Logical. If TRUE then remove \code{NA}s before calculation.
#' @return Numeric.
#' @seealso [stats::sd()]
#' @examples
#' se(1:100)
#' @export

se <- function(
	x,
	na.rm = FALSE
) {
	stats::sd(x, na.rm=na.rm) / sqrt(length(ifelse(na.rm, x, x[!is.na(x)])))
}

