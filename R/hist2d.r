#' Two-dimensional histogram
#'
#' @param x Data frame or matrix with at least two columns. Only frequencies in the first two columns are used.
#' @param breaks1 One of the following describing how breaks for the first variable are calculated:
#' \itemize{
#' \item Numeric vector: Breakpoints for bins for the first variable.
#' \item Single integer: The number of bins into which to tally values of the first variable.
#' \item Function: To compute the vector of breakpoints.
#' \item Function: To compute the number of cells. Used as a suggestion only (see \code{\link[graphics]{hist}}).
#' \item Character: The name of a function to compute the number of cells (see the \emph{Details} section in \code{\link[graphics]{hist}}). Used as a suggestion only (see \code{\link[graphics]{hist}}).
#' }
#' @param breaks2 Same as \code{breaks1} but for the second variable.
#' @param freq Logical, if \code{TRUE} (default) then values in the output represent counts. If \code{FALSE} then they represent relative densities.
#' @param right Logical, if \code{TRUE} (default) then use left-open and right-closed intervals.
#' @param ... Arguments to pass to \code{\link[graphics]{hist}}.
#' @return Object of class \code{matrix} and \code{histogram2d}. Columns pertain to bins of \code{x1} and rows {x2}. Column names and row names are mid-points of bins.
#' @seealso \code{\link[graphics]{hist}}
#' @examples
#' x1 <- rnorm(1000)
#' x2 <- 0.5 * x1 * rnorm(1000)
#' x <- data.frame(x1=x1, x2=x2)
#' hist2d(x)
#' @export
hist2d <- function(
	x,
	breaks1 = 'Sturges',
	breaks2 = 'Sturges',
	freq = TRUE,
	right = TRUE,
	...
) {

	if (!(class(x) %in% c('matrix', 'data.frame'))) stop('Argument "x" in function "hist2d" (omnibus package) must be a matrix or data frame.')
	if (!(class(breaks1) %in% c('character', 'function', 'numeric', 'integer'))) stop('Argument "breaks1" in function "hist2d" (omnibus package) must be a single numeric value, a vector of numeric values, a function, or a character naming a function.')
	if (!(class(breaks2) %in% c('character', 'function', 'numeric', 'integer'))) stop('Argument "breaks2" in function "hist2d" (omnibus package) must be a single numeric value, a vector of numeric values, a function, or a character naming a function.')
	if (!is.logical(freq)) stop('Argument "freq" in function "hist2d" (omnibus package) must be "TRUE" or "FALSE".')
	if (!is.logical(right)) stop('Argument "right" in function "hist2d" (omnibus package) must be "TRUE" or "FALSE".')

	x1 <- x[ , 1]
	x2 <- x[ , 2]
	
	# create breaks for bins for each variable using all available data
	hist1 <- hist(x=x1, breaks=breaks1, plot=FALSE, ...)
	breaks1 <- hist1$breaks
	mids1 <- hist1$mids
	
	hist2 <- hist(x=x2, breaks=breaks2, plot=FALSE, ...)
	breaks2 <- hist2$breaks
	mids2 <- hist2$mids
	
	tallies <- matrix(0, ncol=length(mids1), nrow=length(mids2))
	
	colnames(tallies) <- mids1
	rownames(tallies) <- mids2

	# tally number of records of x2 in each bin of x1
	for (count1 in seq_along(mids1)) {
	
		break1Left <- breaks1[count1]
		break1Right <- breaks1[count1 + 1]
	
		inThisBreak1 <- if (right) {
			which(x1 > break1Left & x1 <= break1Right)
		} else {
			which(x1 >= break1Left & x1 < break1Right)
		}

		# if any records of x2 in this x1 bin
		if (length(inThisBreak1) > 0) {
		
			x2InThisX1 <- x2[inThisBreak1]
			thisHist2 <- hist(x2InThisX1, breaks=breaks2, freq=NULL, plot=FALSE, ...)
			tallies[ , count1] <- thisHist2$counts
			
		}
		
	}
	
	if (!freq) tallies <- tallies / sum(tallies)
	
	class(tallies) <- c('matrix', 'histogram2d')
	attr(tallies, 'breaks1') <- breaks1
	attr(tallies, 'breaks2') <- breaks2
	attr(tallies, 'right') <- right
	attr(tallies, 'equidist1') <- hist1$equidist
	attr(tallies, 'equidist2') <- hist2$equidist
	tallies
	
}
