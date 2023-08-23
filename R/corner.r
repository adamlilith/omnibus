#' Corner of a matrix or data frame
#'
#' Return a corner of a matrix or data frame (i.e., upper left, upper right, lower left, lower right).
#'
#' @param x Data frame or matrix.
#' @param corner Integer in the set {\code{1, 2, 3, 4}} or character in the set {\code{'topleft', 'topright', 'bottomleft', 'bottomright'}} or in the set {\code{'tl', 'tr', 'bl', 'br'}}. Indicates which corner to return. Integers 1, 2, 3 and 4 correspond to top left, top right, bottom left, and bottom right corners. The default is 1, the top left corner.
#' @param size Positive integer, number of rows and columns to return. If there are fewer columns/rows than indicated then all columns/rows are returned.
#' @return A matrix or data frame.
#' @seealso \code{\link[utils]{head}}, \code{\link[utils]{tail}}
#' @examples
#' x <- matrix(1:120, ncol=12, nrow=10)
#' x
#' corner(x, 1)
#' corner(x, 2)
#' corner(x, 3)
#' corner(x, 4)
#' @export
corner <- function(
	x,
	corner = 1,
	size = 5
) {

	corner <- as.character(corner)
	if (corner %in% c('1', 'tl')) corner <- 'topleft'
	if (corner %in% c('2', 'tr')) corner <- 'topright'
	if (corner %in% c('3', 'bl')) corner <- 'bottomleft'
	if (corner %in% c('4', 'br')) corner <- 'bottomright'

	nrows <- nrow(x)
	ncols <- ncol(x)

	showRows <- min(nrows, size)
	showCols <- min(ncols, size)

	out <- if (corner == 'topleft') {
		x[1:showRows, 1:showCols, drop=FALSE]
	} else if (corner == 'topright') {
		x[1:showRows, (ncols - showCols + 1):ncols, drop=FALSE]
	} else if (corner == 'bottomleft') {
		x[(nrows - showRows + 1):nrows, 1:showCols, drop=FALSE]
	} else if (corner == 'bottomright') {
		x[(nrows - showRows + 1):nrows, (ncols - showCols + 1):ncols, drop=FALSE]
	}

	out

}
