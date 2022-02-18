#' Calculate pairwise distances between two matrices or data frames.
#'
#' This function takes two data frames or matrices and returns a matrix of pairwise Euclidean distances between the two.
#' @param x1 Data frame or matrix one or more columns wide.
#' @param x2 Data frame or matrix one or more columns wide.
#' @param na.rm Logical, if \code{TRUE} then any rows in \code{x1} or \code{x2} with at least one \code{NA} are removed first.
#' @return Matrix with \code{nrow(x1)} rows and \code{nrow(x2)} columns. Values are the distance between each row of \code{x1} and row of \code{x2}.
#' @seealso \code{\link[stats]{dist}}
#' @examples
#' x1 <- data.frame(x1=1:20, x2=round(100 * rnorm(20)))
#' x2 <- data.frame(x1=sample(1:30, 30), x2=sort(round(100 * rnorm(30))))
#' pairDist(x1, x2)
#' @export
pairDist <- compiler::cmpfun(function(x1, x2, na.rm = FALSE) {

	if (na.rm) {
		x1 <- x1[stats::complete.cases(x1), , drop=FALSE]
		x2 <- x2[stats::complete.cases(x2), , drop=FALSE]
	}

	dists <- matrix(NA, nrow=nrow(x1), ncol=nrow(x2))

	for (i in 1:nrow(x1)) {
		for (j in 1:nrow(x2)) {
			dists[i, j] <- sqrt(rowSums((x1[i, , drop=FALSE] - x2[j, , drop=FALSE])^2))
		}
	}

	dists

})
