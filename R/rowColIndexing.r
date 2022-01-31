#' Convert between row- and column-style indexing of matrices
#'
#' These functions converts index values of cells between row- and column-style indexing of cells in matrices. Column indexing (the default for matrices) has the cell "1" in the upper left corner of the matrix. The cell "2" is below it, and so on. The numbering then wraps around to the top of the next column. Row indexing (the default for rasters, for example), also has cell "1" in the upper left, but cell "2" is to its right, and so on. Numbering then wraps around to the next row.
#' 
#' @param x		Either a matrix or a vector with two values, one for the number of rows and one for the number of columns in a matrix.
#' @param cell	One or more cell indices (positive integers).
#' @param dir	The "direction" in which to convert. If \code{'row'}, it is assumed that \code{cell} is a column-style index and so should be converted to a column index. If \code{'col'}, it is assumed that \code{cell} is a row-style index and so should be converted to a row index.
#'
#' @return One or more positive integers.
#' @examples
#' 
#' x <- matrix('a', nrow=5, ncol=8)
#' rowColIndexing(x, cell=c(1, 6, 20), 'row')
#' rowColIndexing(x, cell=c(1, 6, 20), 'col')
#' 
#' rowColIndexing(c(5, 8), cell=c(1, 6, 20), 'row')
#' rowColIndexing(c(5, 8), cell=c(1, 6, 20), 'col')
#' 
#' @export

rowColIndexing <- function(x, cell, dir) {

	if (inherits(x, 'matrix')) x <- dim(x)
	numCells <- prod(x)
	if (any(cell < 1)) stop('Cells must be positive integers.')
	if (any(cell > numCells)) stop('At least one cell number of larger than the total number of cells in the matrix.')

	rowMat <- matrix(1:numCells, nrow=x[1], ncol=x[2], byrow=TRUE)
	rows <- c(rowMat)

	if (dir == 'row') {
		out <- rows[cell]
	} else if (dir == 'col') {
		out <- match(cell, rows)
	} else {
		stop('Argument "dir" must be "row" or "col".')
	}
	
	out

}
