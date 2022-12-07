#' Insert values into a vector
#'
#' This function inserts values into a vector, lengthening the overall vector. It is different from, say, \code{x[1:3] <- c('a', 'b', 'c')} which simply replaces the values at indices 1 through 3.
#'
#' @param x	Vector of values to insert.
#' @param into vector of values into which to insert \code{x}.
#' @param at Vector of positions (indices) where \code{x} should be inserted. If the length of \code{x} is shorter than the length of \code{at}, then values in \code{x} will be recycled and a warning produced.
#' @param warn If \code{TRUE}, provide warnings.
#'
#' @return Vector.
#'
#' @seealso \code{\link{insertCol}}, \code{\link{insertRow}}
#'
#' @examples

x <- -1:-3
into <- 10:20
at <- c(1, 3, 14)
insert(x, into, at)

insert(-1, into, at)

insert <- function(x, into, at, warn = TRUE) {

	if (length(x) > length(at)) stop('Length of x is longer than the number of indices.')
	if (any(at > length(at) + length(into))) stop(paste('At least one index is too high. The new vector will be', length(x) + length(into), 'elements long.'))

	out <- rep(NA, length(into) + length(at))
	if (length(x) < length(at)) {
	
		if (warn) warning('Length of x is shorter than the length of at. Recycling x.')
		x <- rep(x, length.out = length(at))
	
	}
	
	out[at] <- x
	out[is.na(out)] <- into
	
	out

}
