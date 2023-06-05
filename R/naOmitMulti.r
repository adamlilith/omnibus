#' Remove NAs from one or more equal-length vectors
#'
#' This function removes elements in one or more equal-length vectors in which there is one \code{NA} at that position. For example, if there are three vectors \code{A}, \code{B}, and \code{C}, and \code{A} has an \code{NA} in the first position and \code{C} has an \code{NA} in the third position, then \code{A}, \code{B}, and \code{C} will each have the elements at positions 1 and 3 removed.
#' @param ... Numeric or character vectors.
#' @return List of objects of class \code{...}.
#' @seealso \code{\link[stats]{na.omit}}
#' @examples
#' a <- c(NA, 'b', 'c', 'd', 'e', NA)
#' b <- c(1, 2, 3, NA, 5, NA)
#' c <- c(6, 7, 8, 9, 10, NA)
#' naOmitMulti(a, b, c)
#' @export

naOmitMulti <- function(...) {

	x <- list(...)

	lengths <- sapply(x, length)
	if (all(lengths != min(lengths))) stop('All vectors must have the same length.')

	nas <- lapply(x, FUN=function(u) which(is.na(u)))
	nas <- unlist(nas)
	if (length(nas) > 0) {

		nas <- unique(nas)
		for (i in seq_along(x)) x[[i]] <- x[[i]][-nas]

	}

	if (length(x) == 1) x <- unlist(x)

	x

}
