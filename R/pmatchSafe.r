#' Partial matching of strings with error checking
#'
#' This function is the same as \code{\link{pmatch}}, but it can throw an error instead of \code{NA} if not match is found, and can be forced to throw the error if more than the desired number of matches is found.
#'
#' @param x Character: String to match.
#' @param table Character vector: Values to which to match.
#' @param useFirst Logical: If \code{TRUE}, and there is more than one match for a given \code{x}, then the first value in \code{table} that matches \code{x} will be returned (without an error or warning).
#' @param error Logical: If no match is found, return an error?
#' @param ignoreCase Logical: If \code{TRUE} (default), ignore the case of values in \code{x} and \code{table} when checking for matches.
#' @param nmax Positive numeric integer: Maximum allowable number of matches. If more than this number of matches is found, an error will be thrown (regardless of the value of \code{error}).
#' @param ... Arguments to pass to \code{\link{pmatch}}.
#'
#' @returns One or more of the values in \code{table}.
#'
#' @examples
#' 
#' pmatchSafe('ap', c('apples', 'oranges', 'bananas'))
#' 
#' pmatchSafe('AP', c('apples', 'oranges', 'bananas'))
#' 
#' pmatchSafe('AP', c('apples', 'oranges', 'bananas'),
#'     ignoreCase = FALSE, error = FALSE)
#' 
#' pmatchSafe(c('ba', 'ap'), c('apples', 'oranges', 'bananas'))
#' 
#' # No match:
#' tryCatch(
#'     pmatchSafe('kumquats', c('apples', 'oranges', 'bananas')),
#' 	error = function(cond) FALSE
#' )
#' 
#' pmatchSafe('kumquats', c('apples', 'oranges', 'bananas'), error = FALSE)
#' 
#' pmatchSafe(c('ap', 'corn'), c('apples', 'oranges', 'bananas'), error = FALSE)
#' 
#' # Too many matches:
#' tryCatch(
#'     pmatchSafe(c('ap', 'ba'), c('apples', 'oranges', 'bananas'), nmax = 1),
#' 	error=function(cond) FALSE
#' )
#' 
#' @export 
pmatchSafe <- function(x, table, useFirst = FALSE, error = TRUE, ignoreCase = TRUE, nmax = length(x), ...) {

	if (ignoreCase) {
		x <- tolower(x)
		lowerTable <- tolower(table)
		matches <- .pmatchSafe(x, table = lowerTable, useFirst = useFirst, error = error)
	} else {
		matches <- .pmatchSafe(x, table = table, useFirst = useFirst, error = error)
	}
	
	if (length(matches) > nmax) stop('Only ', nmax, ' match(es) can be returned.')

	if (length(matches) < length(x)) {
		if (error) stop('Cannot find a match. Valid options include: ', paste(table, collapse=', '))
	}

	table[matches]

}

#' @noRd
.pmatchSafe <- function(x, table, useFirst, error) {

	nc <- nchar(x)

	matches <- integer()
	for (i in seq_along(x)) {
	
		theseMatches <- which(x[i] == substr(table, 1L, nc[i]))

		if (length(theseMatches) == 0L & error) stop('Cannot find a match.')

		if (useFirst) theseMatches <- theseMatches[1L]
		matches <- c(matches, theseMatches)
	
	}

	matches

}
