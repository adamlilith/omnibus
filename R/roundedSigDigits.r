#' Number of significant digits in rounded numbers
#'
#' This function examines a numeric value (typically with numbers after the decimal place) and estimates either:
#' \itemize{
#'		\item The number of significant digits of the numerator and denominator of a fraction that would (approximately) result in the given value.
#' 		\item The number of digits to which an integer may have been rounded, depending on whether the input has values after the decimal place or is an integer. Negative values are treated as positive so the negative of a number will returns the same value as its positive version. See \emph{Details} for more details. \emph{Obviously, values can appear to be rounded or repeating even when they are not!}
#' }
#' @param x 		Numeric or numeric vector.
#' @param minReps 	Integer. Number of times a digit or sequence of digits that occur after a decimal place needs to be repeated to assume it represents a repeating series and thus is assumed to arise from using decimal places to represent a fraction. Default is 3. For example, if \code{minReps} is 3 then 0.111 would be assumed to represent a repeating value because 1 occurs three times, so -1 would be returned since this decimal can be represented by 1/9 (i.e., division of 1 by a single-digit number). However, if \code{minReps} is 4 then the function would assume that if the value had had four digits, the next digit would not have been a 1, so returns -3 because there are three significant values after the decimal place. When the penultimate digit is >5 and the last digit is equal to the penultimate digit plus 1, then the last digit counts as a repeat of the penultimate digit.  So 0.067 is assumed to have two repeating 6s.  If \code{minReps} is 0 or 1 then the function will (usually) return the negative of the total number of decimal places in the value.
#'
#' @details For values with at least one non-zero digit after a decimal place with no repeated series of digits detected, the function simply returns the total number of digits (ignoring trailing zeros) times -1. For example:
#' \itemize{
#'		\item 0.3 returns -1 because there is just one value after the decimal.
#'		\item 0.34567 returns -5 because there are no repeats up to the 5th decimal place.
#'		\item 0.1212125 returns -7 because there are no repeats (starting from the right) up to the 7th decimal place.
#'		\item 0.111117 returns -6 because there are no repeats (starting from the right) up to the 7th decimal place.
#' }
#' The function takes account of rounding up:
#' \itemize{
#'		\item 0.666 might be a truncated version of 2/3. Two and three each have 1 significant digit, so the function returns -1 (1 value after the decimal place).
#'		\item 0.667 also returns -1 because this might represent a rounding of 2/3 and it is customary to round digits up if the next digit would have been >5.
#'		\item 0.3334 returns -4 because it is inappropriate to round 3 up to 4 if the next digit would have been 5 or less.
#' }
#'
#' Repeating series are accounted for. For example:
#' \itemize{
#'		\item 0.121212 returns -2 because "12" starts repeating after the second decimal place.
#'		\item 0.000678678678 returns -6 because "678" starts repeating after the 6th place.
#'		\item 0.678678678 returns -3.
#'		\item 0.678678679 also returns -3 because 678 could be rounded to 679 if the next digit were 6.
#' }
#'
#' Note that you can set the minimum number of times a digit or series needs to be repeated to count as being repeated using the argument \code{minReps}. The default is 3, so digits or series of digits need to be repeated at least 3 times to count a repetition, but this can be changed:
#' \itemize{
#'		\item 0.1111 returns -1 using the default requirement for 3 repetitions but -4 if the number of minimum repetitions is 5 or more.
#'		\item 0.121212 returns -2 using the default requirement for 3 repetitions but -6 if the number of minimum repetitions is 4 or more.
#' }
#'
#' Trailing zeros are ignored, so 0.12300 returns -3. When values do not have digits after a decimal place the location of the first non-zero digit from the right is returned as a positive integer. For example:
#' \itemize{
#'		\item 234 returns 1 because the first non-zero digit from the right is in the 1s place.
#'		\item 100 return 3 because the first non-zero digit from the right is in the 100s place.
#'		\item 70001 returns 1 because the first non-zero digit from the right is in the 1s place.
#' }
#'
#' However, note a few oddities:
#' \itemize{
#'	\item 4E5 returns 6 but 4E50 probably will not return 51 because many computers have a hard time internally representing numbers that large.
#'	\item 4E-5 returns -5 but  probably will not return -50 because many computers have a hard time internally representing numbers that small.
#'	\item -100 and 100 return 3 and -0.12 and 0.12 return -2 because the negative sign is ignored.
#'	\item 0 returns 0.
#'	\item \code{NA} and \code{NaN} returns \code{NA}.
#' }
#' @return Integer (number of digits) or \code{NA} (does not appear to be rounded).
#' @examples
#' roundedSigDigits(0.3)
#' roundedSigDigits(0.34567)
#' roundedSigDigits(0.1212125)
#' roundedSigDigits(0.111117)
#' roundedSigDigits(0.666)
#' roundedSigDigits(0.667)
#' roundedSigDigits(0.3334)
#' roundedSigDigits(0.121212)
#' roundedSigDigits(0.000678678678)
#' roundedSigDigits(0.678678678)
#' roundedSigDigits(0.678678679)
#' roundedSigDigits(0.1111)
#' roundedSigDigits(0.1111, minReps=5)
#' roundedSigDigits(0.121212)
#' roundedSigDigits(0.121212, minReps=4)
#' roundedSigDigits(234)
#' roundedSigDigits(100)
#' roundedSigDigits(70001)
#' roundedSigDigits(4E5)
#' roundedSigDigits(4E50)
#' roundedSigDigits(4E-5)
#' roundedSigDigits(4E-50)
#' roundedSigDigits(0)
#' roundedSigDigits(NA)
#'
#' x <- c(0.0667, 0.0667, 0.067)
#' roundedSigDigits(x)
#' @export
roundedSigDigits <- compiler::cmpfun(function(x, minReps = 3) {

	opts <- options(scipen = 999)
	on.exit(options(opts), add = TRUE)

	n <- length(x)
	out <- rep(NA, n)
	numDecDigits <- countDecDigits(x)
	
	x <- as.character(abs(x))
	x <- strsplit(x, '\\.')
	xBeforeDec <- sapply(x, '[', 1)
	xAfterDec <- sapply(x, '[', 2)
	
	for (i in seq_along(x)) {

		# get segments before/after decimal place
		before <- xBeforeDec[i]
		after <- xAfterDec[i]
		thisDigits <- numDecDigits[i]
	
		if (before != 'NaN' & (!is.na(before) | !is.na(after))) {
		
			# no values after decimal place
			if (is.na(after)) {
				
				# value is 0
				if (as.numeric(before) == 0) {
					out[i] <- 0
				# value is not zero
				} else {
					thisRun <- rle(unlist(strsplit(before, '')))
					if (utils::tail(thisRun$values, 1) == '0') {
						out[i] <- utils::tail(thisRun$lengths, 1) + 1
					} else {
						out[i] <- 1
					}
				}
				
			# values occur after decimal place
			} else {
					
				if (thisDigits == 1) {
					out[i] <- -1
				} else {
				
					thisRun <- rle(unlist(strsplit(after, '')))
					numSets <- length(thisRun$lengths)
					lastRun <- thisRun$lengths[numSets]
					lastVal <- as.numeric(thisRun$values[numSets])

					if (length(thisRun$lengths) > 1) {
						penultRun <- thisRun$lengths[numSets - 1]
						penultVal <- as.numeric(thisRun$values[numSets - 1])
					} else {
						penultRun <- 0
						penultVal <- -Inf
					}

					# default: assume rounded to number of decimal places
					out[i] <- -1 * thisDigits
					
					# last value repeated
					if (lastRun >= minReps) {
						out[i] <- max(out[i], -1 * (thisDigits - lastRun + 1))
					# last value is one more than penultimate value AND penultimate value is >5 and repeated
					} else if (lastVal == penultVal + 1 & penultRun + lastRun >= minReps & penultVal > 5) {
						out[i] <- max(out[i], -1 * (thisDigits - lastRun - penultRun + 1))
					}
					
					### detect repeated series
					if (thisDigits > 1) {
						
						halfLength <- max(2, floor(thisDigits / 2)) #!!!
						seriesLengths <- 2:halfLength
						
						# vary length of series
						for (seriesLength in seriesLengths) {
						
							series <- substr(after, thisDigits - seriesLength + 1, thisDigits)
							
							thisAfter <- after
					
							# how many times series is repeated without interruption starting at right-most decimal place
							seriesReps <- 0
							while (substr(thisAfter, nchar(thisAfter) - seriesLength + 1, nchar(thisAfter)) == series) {
								thisAfter <- substr(thisAfter, 1, nchar(thisAfter) - seriesLength)
								seriesReps <- seriesReps + 1
							}
							
							# is series was repeated
							if (seriesReps >= minReps) {

								delta <- if (substr(series, nchar(series), nchar(series)) == substr(thisAfter, nchar(thisAfter), nchar(thisAfter))) {
									1
								} else {
									0
								}
							
								place <- -1 * (nchar(thisAfter) + seriesLength - delta)
								if (place < 0) out[i] <- max(out[i], place)
								
							}
					
						} # try series of next-longer length
			
						# if last value is rounded up, try series with it decremented by 1
						# if (lastRun == 1 & lastVal == penultVal + 1 & penultVal > 5) {
						if (lastRun == 1) {

							for (seriesLength in seriesLengths) {
							
								series <- substr(after, thisDigits - seriesLength + 1, thisDigits - 1)
								series <- paste(series, lastVal - 1, sep='')

								thisAfter <- substr(after, 1, thisDigits - 1)
								thisAfter <- paste(thisAfter, lastVal - 1, sep='')

								# how many times series is repeated without interruption starting at right-most decimal place
								seriesReps <- 0
								while (substr(thisAfter, nchar(thisAfter) - seriesLength + 1, nchar(thisAfter)) == series) {
									thisAfter <- substr(thisAfter, 1, nchar(thisAfter) - seriesLength)
									seriesReps <- seriesReps + 1
								}
								
								# is series was repeated
								if (seriesReps >= minReps) {

									delta <- if (substr(series, nchar(series), nchar(series)) == substr(thisAfter, nchar(thisAfter), nchar(thisAfter))) {
										1
									} else {
										0
									}
								
									place <- -1 * (nchar(thisAfter) + seriesLength - delta)
									if (place < 0) out[i] <- max(out[i], place)
									
								}
						
							} # try series of next-longer length
							
						} # if last value is rounded up, try series with it decremented by 1
						
					} # detect repeating series
						
				} # more than one value after decimal place
				
			} # after decimal place
			
		} # value is not NA
		
	} # next value

	out
	
})
