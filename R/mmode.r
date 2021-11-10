#' Modal value(s)
#'
#' Modal value. If there is more than one unique mode, all modal values are returned.
#' @param x Numeric or character vector.
#' @return Numeric or character vector.
#' @examples
#' x <- c(1, 2, 3, 3, 4, 5, 3, 1, 2)
#' mmode(x)
#'
#' x <- c(1, 2, 3)
#' mmode(x)
#'
#' @export
mmode <- function(x) {

	uniques <- unique(x)
	tab <- tabulate(match(x, uniques))
	out <- uniques[tab == max(tab)]
	out
}
