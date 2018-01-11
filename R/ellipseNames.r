#' Get names of objects input as arguments in ellipse (\code{...}) form.
#'
#' This function returns the names of objects input into a function as ellipses. It is only useful if called inside a function.
#' @param ... Objects.
#' @return Character list.
#' @export
ellipseNames <- function(...) {

	ellipses <- as.list(substitute(list(...)))[-1L]
	inNames <- character()
	for (i in seq_along(ellipses)) inNames <- c(inNames, as.character(ellipses[[i]]))
	inNames
	
}
