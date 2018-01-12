#' Calculate pairwise distances between two matrices or data frames
#'
#' This function takes two data frames or matrices and returns a matrix of Euclidean distances between the two.
#' @param set1 Data frame or matrix one or more columns wide.
#' @param set2 Data frame or matrix one or more columns wide.
#' @param na.rm Logical, if \code{TRUE} then any rows in \code{set1} or \code{set2} with at least one \code{NA} are removed first.
#' @return Matrix with \code{nrow(set1)} rows and \code{nrow(set2)} columns.
#' @seealso \code{\link[stats]{dist}}
#' @examples
#' set1 <- data.frame(x1=1:20, x2=round(100 * rnorm(20)))
#' set2 <- data.frame(x1=sample(1:30, 30), x2=sort(round(100 * rnorm(30))))
#' pairDist(set1, set2)
#' @export
pairDist <- function(set1, set2, na.rm = FALSE) {

	if (na.rm) {
		if (length(naRows(set1)) > 0) set1 <- set1[-naRows(set1), , drop=FALSE]
		if (length(naRows(set2)) > 0) set2 <- set1[-naRows(set2), , drop=FALSE]
	}

	dists <- matrix(NA, nrow=nrow(set1), ncol=nrow(set2))

	for (i in 1:nrow(set1)) {
		for (j in 1:nrow(set2)) {
			dists[i, j] <- sqrt(rowSums((set1[i, , drop=FALSE] - set2[j, , drop=FALSE])^2))
		}
	}

	dists

}


