#' Left/right side of a data frame or matrix
#'
#' This function extracts the leftmost or rightmost set of columns of a data frame or matrix.
#'
#' @param x A \code{data.frame} or \code{matrix}.
#'
#' @param side Either 1 (left side) or 2 (right side), or \code{'left'} or \code{'right'}. Case is ignored and partial matching is used.
#'
#' @param n Number of columns. The default is 3.
#'
#' @returns A \code{data.frame} or \code{matrix}.
#'
#' @seealso \code{\link{corner}}, \code{\link[utils]{head}}, \code{\link[utils]{tail}}
#'
#' @examples
#'
#' side(iris)
#' side(iris, 2)
#' side(iris, 'l')
#' side(iris, 'r')
#' side(iris, 1, 2)
#'
#' @export
side <- function(x, side = 1, n = 3) {

	if (is.character(side)) side <- pmatch(side, c('left', 'right'))
	
	nc <- ncol(x)
	if (n > nc) n <- nc
	if (side == 1) {
		x[ , 1:n, drop = FALSE]
	} else if (side == 2) {
		x[ , (nc - n + 1):nc, drop = FALSE]
	} else {
		stop('Argument `side` must be 1, 2, or `left` or `right`.')
	}

}
