#' Rename columns
#'
#' This function is short-hand for: \code{colnames(x)[colnames(x) %in% c('old_name_1', 'old_name_2')] <- c('new_name_1', 'new_name_2')}.
#'
#' @param x A \code{data.frame} or \code{matrix}.
#' @param old Character vector with names(s), or numeric vector of the indices of the column(s) you want to rename.
#' @param new Character vector of names.
#'
#' @returns A \code{data.frame} or \code{matrix}.
#'
#' @examples 
#' 
#' x <- data.frame(old_x = 1:5, old_y = letters[1:5], old_z = LETTERS[1:5])
#' x
#' renameCol(x, c('old_y', 'old_z'), c('new_Y', 'new_Z'))
#' renameCol(x, c(2, 3), c('new_Y', 'new_Z')) # same as above
#' 
#' @export
renameCol <- function(x, old, new) {

	if (!inherits(x, c('data.frame', 'matrix'))) stop('The input must be a data.frame, matrix, or inherit one of these classes.')
	if (length(old) != length(new)) stop('The `old` and `new` arguments must have the same length.')

	if (inherits(old, c('numeric', 'integer'))) {
		if (!all(old %in% 1:ncol(x))) stop('One of the values of `x` specifies a column that does not exist in `x`.')
		colnames(x)[old] <- new
	} else {
		xNames <- colnames(x)
		if (!all(old %in% xNames)) stop('All values in `old` must occur in the column names of `x`.')
		colnames(x)[xNames %in% old] <- new
	}

	x

}