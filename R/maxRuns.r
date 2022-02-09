#' Maximum number of continuous "runs" of values meeting a particular condition
#'
#' Consider an ordered set of values, say {1, 4, 0, 0, 0, 2, 0, 10}. We can ask, what is the number of times in which zeroes appear successively? In this example, we have one set of three continuous zeros, and one set of a single zero. So the number of runs with zero is 2, and the maximum run length is 3. This function calculates the number of runs based on a certain condition for defining the run. The condition is stated as a function that returns a logical value. The function for this example would be \code{function(x) x == 0}.
#'
#' @param x Vector of numeric, character, or other values.
#' @param fx A function that returns \code{TRUE}, \code{FALSE}, or (optionally) \code{NA}. The function must use \code{x} as its first argument. For example, \code{function(x) x == 0} is allowable, but \code{function(y) y == 0} is not. Values that count as \code{TRUE} will be counted toward a run.
#' @param args A \emph{list} object with additional arguments to supply to the function \code{fx}.
#' @param failIfAllNA If \code{TRUE}, fail if all values are \code{NA} after being evaluated by \code{fx}.
#'
#' @return Lengths of successive runs of elements that meet the criterion. A single value of 0 indicates no conditions meet the criterion.
#' @examples
#'
#' x <- c(1, 4, 0, 0, 0, 2, 0, 10)
#' fx <- function(x) x == 0
#' maxRuns(x, fx)
#' 
#' fx <- function(x) x > 0
#' maxRuns(x, fx)
#'  
#' fx <- function(x) x > 0 & x < 5
#' maxRuns(x, fx)
#' 
#' x <- c(1, 4, 0, 0, 0, 2, 0, 10)
#' fx <- function(x, th) x == th
#' maxRuns(x, fx, args=list(th=0))
#' 
#' # "count" NA as an observation 
#' x <- c(1, 4, 0, 0, 0, NA, 0, 10)
#' fx <- function(x, th) ifelse(is.na(x), FALSE, x == th)
#' maxRuns(x, fx, args=list(th=0))
#'  
#' # include NAs as part of a run
#' x <- c(1, 4, 0, 0, 0, NA, 0, 10)
#' fx <- function(x, th) ifelse(is.na(x), TRUE, x == th)
#' maxRuns(x, fx, args=list(th=0))
#'  
#' @export
maxRuns <- function(x, fx, args=NULL, failIfAllNA = FALSE) {

	theseArgs <- c(list(x=x), args)
	y <- do.call(fx, args=theseArgs)

	if (all(is.na(y))) {
		if (failIfAllNA) {
			stop('All evaluated values are NA.')
		} else {
			out <- 0
		}
	} else {
		
		rl <- rle(y)
		whichMeetCriteria <- which(!is.na(rl$values) & rl$values)
		out <- if (length(whichMeetCriteria) == 0) {
			0
		} else {
			rl$lengths[whichMeetCriteria]
		}
	}
	
	out <- max(out)
	out

}
