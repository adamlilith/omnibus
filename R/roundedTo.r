#' Number of digits to which a value if rounded (if it is)
#'
#' This function calculates the digits location of values that might be rounded. Trailing zeros are ignored. See \emph{Details}.
#' @param x Numeric or numeric vector.
#' @param requireRepeatPenult Logical, if \code{TRUE} (default), then for numbers with non-zero digits after the decimal, the penultimate digits must be repeated at least twice when it is >5 and equal to the last digit minus 1 for the penultimate digit to determine the number of decimal places.  See \emph{Details}.
#' @details
#' This function returns a value that depends on whether:
#' \itemize{
#' 		\item \code{x} has no digits after the decimal place and is not equal to 0: In this case, the value returned is a positive integer indicating the location of the first non-zero digit starting from the right. For example, 1500 returns 3 because the first non-zero digit from the right occurs in the 100s place, and 15000 returns 4, and 15 returns 1.
#' 		\item \code{x} equals 0: The value returned is 0.
#'		\item \code{x} has digits after the decimal that are not all 0: In this case the value returned is a negative integer indicating the location of the last non-zero value that is not repeated or does not appear rounded. For example, 0.123 returns -3 because the last non-zero digit is in the thousandths place.  0.0123 returns -4, and 0.1 returns -1.  Trailing zeros are ignored, so 0.1 and 0.1000 both return -1.  If the last digit is repeated, then the returned value is equal to the total number of digits after the decimal (ignoring trailing zeros) minus the number of repeated digits at the end plus 1.  So 0.66 returns -1 and 0.066 returns -2, but 0.062 returns -3 because the last value is not repeated. If the last digit is one more than the penultimate digit and the penultimate digit is >5, then the value returned is the number of digits after the decimal (ignoreing trailing zeros) minus the the run length of the penultimate digit.  So 0.6667 returns -1 because the penultimate digit is >5 and is 1 less than the last digit.  The values 0.034 returns 3 because the penultimate digit is <5 so if it had been repeated it would not have been rounded up to 4.
#' }
#' The argument \code{requireRepeatPenult} determines how rounding is scored when the last digit might appear to be rounded up. For example, if \code{requireRepeatPenult} is \code{TRUE} (default), then -3 will be returned for 0.067 because even though this might appear to be rounded, the 6 is not repeated.  However, if \code{requireRepeatPenult} is \code{FALSE}, then 0.067 would return -2 because the 6 would not need to be repeated for it to appear to have been rounded to 7 in the thousandths place.  Note that this argument only applies to cases where the penultimate digit is >5.  Otherwise it is ignored. For example, 0.0335 would return -4 regardless of the value of \code{requireRepeatPenult}.
#' Obviously, values can appear to be rounded even when they are not!
#' @return Integer (number of digits) or \code{NA} (does not appear to be rounded).
#' @examples
#' x <- c(1, 1.1, 1.01, 1.0, 1.10, 0.66, 0.67,
#' 0.62, 10, 100, 1000, 1100, 1100.1)
#' roundedTo(x)
#'
#' x <- c(0.0667, 0.0667, 0.067)
#' roundedTo(x)
#' roundedTo(x, FALSE)
#' @export
roundedTo <- function(x, requireRepeatPenult = TRUE) {

	n <- length(x)
	out <- rep(NA, n)
	numDecDigits <- countDecDigits(x)
	
	x <- as.character(abs(x))
	x <- strsplit(x, '\\.')
	xBeforeDec <- sapply(x, '[', 1)
	xAfterDec <- sapply(x, '[', 2)
	
	for (i in seq_along(x)) {
	
		before <- xBeforeDec[i]
		after <- xAfterDec[i]
	
		if (!is.na(before) | !is.na(after)) {
		
			# no values after decimal place
			if (is.na(after)) {
				
				# value is 0
				if (as.numeric(before) == 0) {
					out[i] <- 0
				# value is not zero
				} else {
					runLength <- rle(unlist(strsplit(before, '')))
					if (tail(runLength$values, 1) == '0') {
						out[i] <- tail(runLength$lengths, 1) + 1
					} else {
						out[i] <- 1
					}
				}
				
			# values occur after decimal place
			} else {
					
				if (numDecDigits[i] == 1) {
					out[i] <- -1
				} else {
				
					runLength <- rle(unlist(strsplit(after, '')))
					tailRun <- tail(runLength$lengths, 1)
					penultRun <- tail(runLength$lengths, 2)[1]
					
					lastVal <- as.numeric(runLength$values[length(runLength$values)])
					penultVal <- as.numeric(runLength$values[length(runLength$values) - 1])

					### detect repeated series
					if (length(runLength$values) > 1) {
						
						leftestPlace <- -Inf # tracks left-most place across series of different lengths
						
						halfLength <- floor(numDecDigits[i] / 2)
						seriesLengths <- 2:halfLength
						
						# vary length of series
						for (seriesLength in seriesLengths) {
						
							series <- substr(after, nchar(after) - seriesLength + 1, nchar(after))
							
							thisAfter <- after
					
							seriesReps <- 0
							while (substr(thisAfter, nchar(thisAfter) - seriesLength + 1, nchar(thisAfter)) == series) {
								thisAfter <- substr(thisAfter, 1, nchar(thisAfter) - seriesLength)
								seriesReps <- seriesReps + 1
							}

							place <- -1 * ((seriesReps - 1) * seriesLength)
							if (place != 0) leftestPlace <- max(leftestPlace, place)
					
						} # try series of next-longer length
			
						# if last value is rounded up, try series with it decremented by 1
						if (lastVal == penultVal + 1 & penultVal > 5) {

							# vary length of series
							for (seriesLength in seriesLengths) {

								series <- substr(after, nchar(after) - seriesLength + 1, nchar(after) - 1)
								series <- paste(series, lastVal - 1, sep='')
							
								thisAfter <- after
					
								seriesReps1 <- 0
								while (substr(thisAfter, nchar(thisAfter) - seriesLength + 1, nchar(thisAfter)) == series) {
									thisAfter <- substr(thisAfter, 1, nchar(thisAfter) - seriesLength)
									seriesReps1 <- seriesReps1 + 1
								}
								
								place <- -1 * (seriesReps1 * seriesLength)
								if (place != 0) leftestPlace <- max(leftestPlace, place)

							} # try series of next-longer length

						}
					
						if (!is.infinite(leftestPlace)) {
							out[i] <- leftestPlace
						}
						
					} # detect repeating series
						
					# last value repeated
					if (tailRun > 1) {
						out[i] <- max(out[i], -1 * (numDecDigits[i] - tailRun + 1), na.rm=TRUE)
					
					# last value is one more than penultimate value AND penultimate value is >5 and repeated
					} else if (requireRepeatPenult & lastVal == penultVal + 1 & penultRun > 1 & penultVal > 5) {
						out[i] <- max(out[i], -1 * (numDecDigits[i] - tailRun - penultRun + 1), na.rm=TRUE)

					# last value is not repeated AND is one more than penultimate value AND penultimate value is >5
					} else if (!requireRepeatPenult & lastVal == penultVal + 1 & penultVal > 5) {
						out[i] <- max(out[i], -1 * (numDecDigits[i] - tailRun - penultRun + 1), na.rm=TRUE)
					
					# detect repeated series
					} else {
						out[i] <- max(out[i], -1 * numDecDigits[i], na.rm=TRUE)
					}
					
				} # more than one value after decimal place
				
			} # after decimal place
			
		} # value is not NA
		
	} # next value

	out
	
}
