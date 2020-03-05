#' Is a year a leap year?
#'
#' Returns \code{TRUE} if the year is a leap year. You can use "negative" years for BCE.
#' @param x Integer or vector of integers representing years.
#' @return Vector of logical values.
#' @examples
#' isLeapYear(1990:2004) # note 2000 *was* not a leap year
#' isLeapYear(1896:1904) # 1900 was *not* a leap year
#' @export
isLeapYear <- compiler::cmpfun(function(x) {

	out <- sapply(x, .isLeap)
	out

})

# workhorse
.isLeap <- compiler::cmpfun(function(x) {

	div4 <- (x %% 4 == 0)

	out <- if (!div4) {
		FALSE
	} else {
		div100 <- (x %% 100 == 0)
		if (!div100) {
			TRUE
		} else {
			div400 <- (x %% 400 == 0)
			if (div400) {
				TRUE
			} else {
				FALSE
			}
		}
	}
	
	out
	
})
