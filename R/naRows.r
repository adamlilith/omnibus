#' Index of rows in a data frame or matrix that contain at least one \code{NA}
#'
#' This function returns the row number of any row in a data frame or matrix that has at least one \code{NA}. This is the same as \code{which(!complete.cases(x))}.
#' @param x Data frame or matrix.
#' @param inf Logical, if \code{TRUE} then also return row numbers of rows in which at least one element is \code{Inf} or \code{-Inf}. The default is \code{FALSE}.
#' @param inverse Logical, if \code{TRUE} then return row numbers of rows that \emph{do not} have \code{NA}s (and possibly \code{Inf} or \code{-Inf}).  The default is \code{FALSE}.
#' @return Integer vector.
#' @examples
#' x <- data.frame(a=1:5, b=c(1, 2, NA, 4, 5), c=c('a', 'b', 'c', 'd', NA))
#' naRows(x)
#' @export
naRows <- function(x, inf = FALSE, inverse = FALSE) {

        if (!inherits(x, 'data.frame')) x <- as.data.frame(x)
        
        focalRows <- lapply(x, is.na)
        focalRows <- unlist(lapply(focalRows, which))
        
        if (inf) {
                isInf <- lapply(x, is.infinite)
                isInf <- unlist(lapply(isInf, which))
                focalRows <- c(focalRows, isInf)
        }
        
        if (length(focalRows) > 0) focalRows <- sort(unique(focalRows))
		if (inverse) focalRows <- {1:nrow(x)}[-focalRows]
		
        focalRows
        
}
