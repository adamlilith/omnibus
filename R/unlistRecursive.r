#' For any object in a list that is also a list, unlist it
#'
#' @description This function takes as an argument a \code{list}. If any of its elements are also lists, it unlists them. The output is the same as the input, except that there will be one new element per element in each sublist, and the sublists will be removed.
#'
#' @param x A \code{list}.
#'
#' @returns A \code{list}.
#'
#' @seealso \code{\link{unlist}}
#'
#' @examples
#'
#' x <- list(
#'    a = 1:3,
#'    b = list(
#'       b1 = c("The", "quick", "brown", "function"),
#' 	     b2 = 4:1,
#'       b3 = list(
#'          b3_1 = 5:7
#'       )
#'    ),
#'    c = "end"
#' )
#'
#' unlistRecursive(x)
#'
#' @export
unlistRecursive <- function(x) {

	if (any(sapply(x, is.list))) {
		out <- list()
		for (i in seq_along(x)) {
			if (is.list(x[[i]])) {
				out <- c(out, unlistRecursive(x[[i]]))
			} else {
				out <- c(out, x[[i]])
			}
		}
		x <- out
	}
	x
}
