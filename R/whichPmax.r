#' Which vector has maximum value at each element
#'
#' This function is a vectorized version of \code{which.max}, which returns the index of the value that is maximum (or the first maximum value, if there is a tie). In this case, the function is supplied two or more vectors of the same length. For each element at the same position (e.g., the first element in each vector, then the second element, etc.) the function returns an integer indicating which vector has the highest value (or the index of the first vector with the highest value in case of ties).
#' @param ... Two or more vectors. If lengths do not match, the results will likely be be unanticipated.
#' @param na.rm Logical, if \code{FALSE} and any of the vectors contains an \code{NA} or \code{NaN}, the function will return an \code{NA}. If \code{TRUE} (default), then \code{NA} will only be returned if all elements at that position are \code{NA} or \code{NaN}.
#' @return Vector the same length as the input, with numeric values indicating which vector has the highest value at that position. In case of ties, the index of the first vector is returned.
#' @seealso \code{\link{which.max}}, \code{\link{which.min}}, \code{\link{whichPMin}}, \code{\link{pmax}}, \code{\link{pmin}}
#' @examples
#' set.seed(123)
#' a <- sample(9, 5)
#' b <- sample(9, 5)
#' c <- sample(9, 5)
#' a[2:3] <- NA
#' b[3] <- NA
#' a[6] <- NA
#' b[6] <- NA
#' c[6] <- NA
#' whichPMax(a, b, c)
#' whichPMax(a, b, c, na.rm=FALSE)
#' @export
whichPMax <- function(..., na.rm = TRUE) {

	# convert to list
	x <- list(...)

	# check for same length
	sameLength <- TRUE
	if (!sameLength) {
		for (i in 1:(length(x) - 1)) {
			if (length(x[[i]]) != length(x[[i + 1]])) {
				sameLength <- FALSE
			}
		}
	}
	
	if (!sameLength) warning('Vectors have different length.')
	
	# which.pmax
	x <- sapply(x, as.matrix)
	out <- apply(x, 1, which.max)
	zeroLengths <- which(sapply(out, length) == 0)
	if (length(zeroLengths) > 0) for (i in zeroLengths) out[[i]] <- NA
	out <- unlist(out)
	
	# implant NAs
	if (!na.rm & anyNA(x)) {
		nas <- which(is.na(rowSums(x)))
		out[nas] <- NA
	}

	out

}
