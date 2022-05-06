#' Calculate pairwise distances between two matrices or data frames.
#'
#' This function takes two data frames or matrices and returns a matrix of pairwise Euclidean distances between the two.
#' @param x1 Data frame or matrix one or more columns wide.
#' @param x2 Data frame or matrix one or more columns wide. If \code{NULL}, then pairwise distances between all points in \code{x1} are calculated.
#' @param na.rm Logical, if \code{TRUE} then any rows in \code{x1} or \code{x2} with at least one \code{NA} are removed first.
#' @return Matrix with \code{nrow(x1)} rows and \code{nrow(x2)} columns. Values are the distance between each row of \code{x1} and row of \code{x2}.
#' @seealso \code{\link[stats]{dist}}
#' @examples
#'
#' x1 <- data.frame(x=sample(1:30, 30), y=sort(round(100 * rnorm(30))), z=sample(1:30, 30))
#' x2 <- data.frame(x=1:20, y=round(100 * rnorm(20)), z=sample(1:20, 20))
#' pairDist(x1, x2)
#' pairDist(x1)
#'
#' @export
pairDist <- compiler::cmpfun(function(x1, x2 = NULL, na.rm = FALSE) {

	if (is.null(x2)) x2 <- x1

	if (na.rm) {
		x1 <- x1[stats::complete.cases(x1), , drop=FALSE]
		x2 <- x2[stats::complete.cases(x2), , drop=FALSE]
	}

	dists <- matrix(NA, nrow=nrow(x1), ncol=nrow(x2))

	for (i in 1:nrow(x1)) {
		x <- unlist(x1[i, , drop=TRUE])
		for (j in 1:nrow(x2)) {
			dists[i, j] <- sqrt(sum((x - unlist(x2[j, ]))^2))
		}
	}

	dists

})
