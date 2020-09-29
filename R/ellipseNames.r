#' Get names of objects input as arguments in ellipse (\code{...}) form
#'
#' This function returns the names of objects input into a function as ellipses. It is only useful if called inside a function.
#' @param ... Objects.
#' @return Character list.
ellipseNames <- function(...) {

	ellipses <- as.list(substitute(list(...)))[-1L]
	inNames <- rep(NA, length(ellipses))
	for (i in seq_along(ellipses)) inNames[i] <- as.character(ellipses[[i]])
	inNames

}
