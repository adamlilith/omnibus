#' Flip an object
#'
#' This function creates a "mirror" image of a character string, a number, a matrix, or a data frame. For example "Shots were exchanged at the hospital" becomes "latipsoh eht ta degnahcxe erew stohS' and 3.14159 becomes 95141.3. Data frames and matrices will be returned with the order of columns or order of rows reversed.
#' @param x A vector of numeric or character values, or a matrix or data frame.
#' @param direction Only used if \code{x} is a matrix or data frame. Accepted values are \code{'lr'} (left-right mirror) or \code{'ud'} (up-down mirror).
#' @returns Object with same class as \code{x}.
#' @examples
#' x <- 'Shots were exchanged at the hospital'
#' mirror(x)
#'
#' x <- c('Water', 'water', 'everywhere')
#' mirror(x)
#'
#' # last value will return NA because the exponentiation does not 
#' # make sense when written backwards
#' x <- c(3.14159, 2.71828, 6.02214076e+23)
#'
#' mirror(x)
#' x <- data.frame(x=1:5, y=6:10)
#' mirror(x)
#'
#' x <- matrix(1:10, nrow=2)
#' mirror(x)
#' @export
mirror <- function(x, direction='lr') {

	xClass <- class(x)
	
	if (inherits(x, c('numeric', 'integer', '64bit'))) {
		x <- as.character(x)
	}

	if (inherits(x, c('character', 'numeric', 'integer', '64bit'))) {
		
		x <- strsplit(x, '')
		x <- lapply(x, rev)
		x <- sapply(x, paste, collapse='')
		
	} else if (inherits(x, c('matrix', 'data.frame'))) {
	
		x <- if (direction == 'lr') {
			x[ , ncol(x):1]
		} else if (direction == 'ud') {
			x[nrow(x):1, ]
		}

	}
		
	class(x) <- xClass
	x

}
