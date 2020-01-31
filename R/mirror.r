#' Reverse order of characters in a string or digits in a number
#'
#' This function creates a "mirror" image of a character string or a number. For example "Shots were exchanged at the hospital" becomes "latipsoh eht ta degnahcxe erew stohS' and 3.14159 becomes 95141.3.
#' @param x Numeric or character or a vector of numeric or character values.
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
#' mirror(x)
#' export
mirror <- function(x) {

	xClass <- class(x)
	
	if (xClass %in% c('numeric', 'integer', '64bit')) {
		x <- as.character(x)
	}

	x <- strsplit(x, '')
	x <- lapply(x, rev)
	x <- sapply(x, paste, collapse='')

	class(x) <- xClass
	x

}
