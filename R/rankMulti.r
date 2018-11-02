#' A multivariate adaptation of the \code{rank()} function
#'
#' This function ranks values in a data frame or matrix by more than one field, with ties in one field broken by subsequent fields.
#' @param x Data frame or matrix.
#' @param cols Names or indices of columns by which to rank, with first one gaining preference over the second, second over the third, etc.
#' @param ... Arguments to pass to \code{\link[base]{rank}}.  Note that if the \code{ties.method} argument is used the options \code{'first'} or \code{'random'} will rank by the first column uniquely such that there are no ties for subsequent columns to break.
#' @return Nummeric list of ranks.
#' @examples
#' x <- data.frame(x1=c('a', 'b', 'b', 'c', 'a', 'a'), x2=c(11, 2, 1, NA, 10, 11))
#' rankMulti(x)
#' rankMulti(x, c('x2', 'x1'))
#' @export
rankMulti <- function(x, cols=1:ncol(x), ...) {

	# get ranks from first column
	ranks <- rank(x[ , cols[1]], ...)

	# if data frame has more than one column, continue ranking
	if (length(cols) > 1) {

		countCol <- 2

		# if there are any ties in the existing ranking, use next column to try to break ties
		while (length(unique(ranks)) < nrow(x) & countCol <= ncol(x)) {

			newRanks <- rep(NA, nrow(x))

			# assign new ranks for all ranks of same value based on ranks of same records in next column
			for (thisRank in sort(unique(ranks))) {

				# get ranks of next column for records in existing rankings that are tied, assign new ranks based on next column
				newRanks[which(ranks == thisRank)] <- rank(x[which(ranks == thisRank), countCol]) + ifelse(is.infinite(max(newRanks, na.rm=TRUE)), 0, sum(!is.na(newRanks)))

			}

			ranks <- newRanks
			countCol <- countCol + 1

		}

	}

	ranks

}
