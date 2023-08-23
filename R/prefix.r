#' Add leading characters to a string
#'
#' Add leading characters to a string. This function is useful for ensuring, say, files get sorted in a particular order. For example, on some operating systems a file name "file 1" would come first, then "file 10", then "file 11", "file 12", etc., then "file 2", "file 21", and so on. Using \code{prefix}, you can add one or more leading zeros so that file names are as "file 01", "file 02", "file 03", and so on... and they will sort that way.
#' @param x Character or character vector to which to add a prefix.
#' @param len The total number of characters desired for each string. If a string is already this length or longer then nothing will be prefixed to that string.
#' @param pad Character. Symbol to prefix to each string.
#' @return Character or character vector.
#' @examples
#' prefix(1:5, len=2)
#' prefix(1:5, len=5)
#' prefix(1:5, len=3, pad='!')
#' @export

prefix <- function(x, len, pad = '0') {
	
    worker <- function(x, len, pad) {
        
    	x <- as.character(x)
    	size <- nchar(x)
    	if (nchar(x) < len) {
    		addTo <- paste(rep(pad, each=len - size), collapse='')
    		x <- paste0(addTo, x)
    	}
    	return(x)
    }
 
    x <- sapply(FUN=worker, X=x, len=len, pad=pad)
    x

}

