#' Year from date formats that are possibly ambiguous
#'
#' This function attempts to return the year from characters representing dates formats. The formats can be ambigous and varied within the same set.  For example, it returns "1982" (or 9982 if century is ambigous) from "11/20/82", "1982-11-20", "Nov. 20, 1982", "20 Nov 1982", "20-Nov-1982", "20/Nov/1982", "20 Nov. 82", "20 Nov 82". The function handles ambiguous centuries (e.g., 1813, 1913, 2013) by including a dummy place holder for the century place (i.e., 9913). Note that it may return warnings like "NAs introduced by coercion".
#' @param x Character or character list, one or more dates.
#' @param yearLast Logical, if \code{TRUE} assume that dates like "XX/YY/ZZ" list the year last (=ZZ).  If \code{FALSE}, assume they're first (=XX).
#' @return Numeric.
#' @examples
#' yearFromDate(1969, yearLast=TRUE)
#' yearFromDate('10-Jul-71', yearLast=TRUE) # --> 9971
#' yearFromDate('10-Jul-1971', yearLast=TRUE) # --> 1971
#' yearFromDate('10-19-71', yearLast=TRUE) # --> 9971
#' yearFromDate('10-19-1969', yearLast=TRUE) # --> 1969
#' yearFromDate('10-1-71', yearLast=TRUE) # --> 9971
#' yearFromDate('3-22-71', yearLast=TRUE) # --> 9971
#' yearFromDate('3-2-71', yearLast=TRUE) # --> 9971
#' yearFromDate('10-1-1969', yearLast=TRUE) # --> 1969
#' yearFromDate('3-22-1969', yearLast=TRUE) # --> 1969
#' yearFromDate('3-2-1969', yearLast=TRUE) # --> 1969
#' yearFromDate('10/Jul/71', yearLast=TRUE) # --> 9971
#' yearFromDate('10/Jul/1971', yearLast=TRUE) # --> 1971
#' yearFromDate('10/19/71', yearLast=TRUE) # --> 9971
#' yearFromDate('10/19/1969', yearLast=TRUE) # --> 1969
#' yearFromDate('10/1/71', yearLast=TRUE) # --> 9971
#' yearFromDate('3/22/71', yearLast=TRUE) # --> 9971
#' yearFromDate('3/2/71', yearLast=TRUE) # --> 9971
#' yearFromDate('10/1/1969', yearLast=TRUE) # --> 1969
#' yearFromDate('3/22/1969', yearLast=TRUE) # --> 1969
#' yearFromDate('3/2/1969', yearLast=TRUE) # --> 1969
#' yearFromDate('10 mmm 71', yearLast=TRUE) # "mmm" is month abbreviation--> 9971
#' yearFromDate('5 mmm 71', yearLast=TRUE) # "mmm" is month abbreviation--> 9971
#' yearFromDate('10 19 71', yearLast=TRUE) # --> 9971
#' yearFromDate('10 19 1969', yearLast=TRUE) # --> 1969
#' yearFromDate('10 1 71', yearLast=TRUE) # --> 9971
#' yearFromDate('3 22 71', yearLast=TRUE) # --> 9971
#' yearFromDate('3 2 71', yearLast=TRUE) # --> 9971
#' yearFromDate('10 1 1969', yearLast=TRUE) # --> 1969
#' yearFromDate('3 22 1969', yearLast=TRUE) # --> 1969
#' yearFromDate('3 2 1969', yearLast=TRUE) # --> 1969
#' yearFromDate('Oct. 19, 1969', yearLast=TRUE) # --> 1969
#' yearFromDate('19 October 1969', yearLast=TRUE) # --> 1969
#' yearFromDate('How you do dat?', yearLast=TRUE) # --> NA
#' yearFromDate('2014-07-03', yearLast=TRUE) # --> 2014
#' yearFromDate('2014-7-03', yearLast=TRUE) # --> 2014
#' yearFromDate('2014-07-3', yearLast=TRUE) # --> 2014
#' yearFromDate('2014-7-3', yearLast=TRUE) # --> 2014
#' yearFromDate('2014/07/03', yearLast=TRUE) # --> 2014
#' yearFromDate('2014/7/03', yearLast=TRUE) # --> 2014
#' yearFromDate('2014/07/3', yearLast=TRUE) # --> 2014
#' yearFromDate('2014/7/3', yearLast=TRUE) # --> 2014
#' yearFromDate('2014 07 03', yearLast=TRUE) # --> 2014
#' yearFromDate('2014 7 03', yearLast=TRUE) # --> 2014
#' yearFromDate('2014 07 3', yearLast=TRUE) # --> 2014
#' yearFromDate('2014 7 3', yearLast=TRUE) # --> 2014
#'
#' yearFromDate(1969, yearLast=FALSE)
#' yearFromDate('10-Jul-71', yearLast=FALSE) # --> 9971
#' yearFromDate('10-Jul-1971', yearLast=FALSE) # --> 1971
#' yearFromDate('10-19-71', yearLast=FALSE) # --> 9910
#' yearFromDate('10-19-1969', yearLast=FALSE) # --> 1969
#' yearFromDate('10-1-71', yearLast=FALSE) # --> 9910
#' yearFromDate('3-22-71', yearLast=FALSE) # --> 9971
#' yearFromDate('3-2-71', yearLast=FALSE) # --> 9971
#' yearFromDate('10-1-1969', yearLast=FALSE) # --> 1969
#' yearFromDate('3-22-1969', yearLast=FALSE) # --> 1969
#' yearFromDate('3-2-1969', yearLast=FALSE) # --> 1969
#' yearFromDate('10/19/71', yearLast=FALSE) # --> 9910
#' yearFromDate('10/19/1969', yearLast=FALSE) # --> 1969
#' yearFromDate('10/1/71', yearLast=FALSE) # --> 9910
#' yearFromDate('3/22/71', yearLast=FALSE) # --> 9971
#' yearFromDate('3/2/71', yearLast=FALSE) # --> 9971
#' yearFromDate('10/1/1969', yearLast=FALSE) # --> 1969
#' yearFromDate('3/22/1969', yearLast=FALSE) # --> 1969
#' yearFromDate('3/2/1969', yearLast=FALSE) # --> 1969
#' yearFromDate('10 mmm 71', yearLast=FALSE) # "mmm" is month abbreviation--> 9971
#' yearFromDate('5 mmm 71', yearLast=FALSE) # "mmm" is month abbreviation--> 9971
#' yearFromDate('10 19 71', yearLast=FALSE) # --> 9910
#' yearFromDate('10 19 1969', yearLast=FALSE) # --> 1969
#' yearFromDate('10 1 71', yearLast=FALSE) # --> 9910
#' yearFromDate('3 22 71', yearLast=FALSE) # --> 9971
#' yearFromDate('3 2 71', yearLast=FALSE) # --> 9971
#' yearFromDate('10 1 1969', yearLast=FALSE) # --> 1969
#' yearFromDate('3 22 1969', yearLast=FALSE) # --> 1969
#' yearFromDate('3 2 1969', yearLast=FALSE) # --> 1969
#' yearFromDate('Oct. 19, 1969', yearLast=FALSE) # --> 1969
#' yearFromDate('19 October 1969', yearLast=FALSE) # --> 1969
#' yearFromDate('How you do dat?', yearLast=FALSE) # --> NA
#' yearFromDate('2014-07-03', yearLast=FALSE) # --> 2014
#' yearFromDate('2014-7-03', yearLast=FALSE) # --> 2014
#' yearFromDate('2014-07-3', yearLast=FALSE) # --> 2014
#' yearFromDate('2014-7-3', yearLast=FALSE) # --> 2014
#' yearFromDate('2014/07/03', yearLast=FALSE) # --> 2014
#' yearFromDate('2014/7/03', yearLast=FALSE) # --> 2014
#' yearFromDate('2014/07/3', yearLast=FALSE) # --> 2014
#' yearFromDate('2014/7/3', yearLast=FALSE) # --> 2014
#' yearFromDate('2014 07 03', yearLast=FALSE) # --> 2014
#' yearFromDate('2014 7 03', yearLast=FALSE) # --> 2014
#' yearFromDate('2014 07 3', yearLast=FALSE) # --> 2014
#' yearFromDate('2014 7 3', yearLast=FALSE) # --> 2014
#' @export
yearFromDate <- function(x, yearLast = TRUE) {

	x <- as.character(x)
	years <- rep(NA, length(x))

	for (i in seq_along(x)) {

		xChar <- as.character(x[i])
		xNum <- suppressWarnings(as.numeric(x[i]))

		# if NA
		if (is.na(x[i]) | x[i] == 'NA' | x[i] == '0' | nchar(x[i]) == 0 | nchar(x[i]) == 1) {

			years[i] <- NA

		# if just a year is listed
		} else if (!is.na(xNum) && nchar(as.character(xNum))==4 & xNum > 0) {

			years[i] <- xChar

		# if "XX/Mon/ZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==3 & unlist(gregexpr(text=xChar, pattern='/'))[2]==7) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=8, stop=9)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X/Mon/ZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==2 & unlist(gregexpr(text=xChar, pattern='/'))[2]==6) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=8)))

		# if "XX/YY/ZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==3 & unlist(gregexpr(text=xChar, pattern='/'))[2]==6) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=8)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X/Y/ZZ"
		} else if (nchar(xChar)==6 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==2 & unlist(gregexpr(text=xChar, pattern='/'))[2]==4) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=5, stop=6)))

		# if "XX/Y/ZZ"
		} else if (nchar(xChar)==7 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==3 & unlist(gregexpr(text=xChar, pattern='/'))[2]==5) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=7)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X/YY/ZZ"
		} else if (nchar(xChar)==7 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==2 & unlist(gregexpr(text=xChar, pattern='/'))[2]==5) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=7)))

		# if "XX/YY/ZZZZ"
		} else if (nchar(xChar)==10 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==3 & unlist(gregexpr(text=xChar, pattern='/'))[2]==6) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=10)))

		# if "X/Y/ZZZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==2 & unlist(gregexpr(text=xChar, pattern='/'))[2]==4) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=5, stop=8)))

		# if "XX/Y/ZZZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==3 & unlist(gregexpr(text=xChar, pattern='/'))[2]==5) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=9)))

		# if "X/YY/ZZZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==2 & unlist(gregexpr(text=xChar, pattern='/'))[2]==5) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=9)))

		# if "ZZZZ/XX/YY"
		} else if (nchar(xChar)==10 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==5 & unlist(gregexpr(text=xChar, pattern='/'))[2]==8) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ/X/Y"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==5 & unlist(gregexpr(text=xChar, pattern='/'))[2]==7) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ/XX/Y"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==5 & unlist(gregexpr(text=xChar, pattern='/'))[2]==8) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ/X/YY"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='/')))==2 & unlist(gregexpr(text=xChar, pattern='/'))[1]==5 & unlist(gregexpr(text=xChar, pattern='/'))[2]==7) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "XX-Mon-ZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==3 & unlist(gregexpr(text=xChar, pattern='-'))[2]==7) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=8, stop=9)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X-Mon-ZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==2 & unlist(gregexpr(text=xChar, pattern='-'))[2]==6) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=8)))

		# if "XX-YY-ZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==3 & unlist(gregexpr(text=xChar, pattern='-'))[2]==6) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=8)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X-Y-ZZ"
		} else if (nchar(xChar)==6 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==2 & unlist(gregexpr(text=xChar, pattern='-'))[2]==4) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=5, stop=6)))

		# if "XX-Y-ZZ"
		} else if (nchar(xChar)==7 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==3 & unlist(gregexpr(text=xChar, pattern='-'))[2]==5) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=7)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X-YY-ZZ"
		} else if (nchar(xChar)==7 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==2 & unlist(gregexpr(text=xChar, pattern='-'))[2]==5) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=7)))

		# if "XX-YY-ZZZZ"
		} else if (nchar(xChar)==10 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==3 & unlist(gregexpr(text=xChar, pattern='-'))[2]==6) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=10)))

		# if "X-Y-ZZZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==2 & unlist(gregexpr(text=xChar, pattern='-'))[2]==4) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=5, stop=8)))

		# if "XX-Y-ZZZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==3 & unlist(gregexpr(text=xChar, pattern='-'))[2]==5) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=9)))

		# if "X-YY-ZZZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==2 & unlist(gregexpr(text=xChar, pattern='-'))[2]==5) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=9)))

		# if "ZZZZ-XX-YY"
		} else if (nchar(xChar)==10 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==5 & unlist(gregexpr(text=xChar, pattern='-'))[2]==8) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ-X-Y"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==5 & unlist(gregexpr(text=xChar, pattern='-'))[2]==7) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ-XX-Y"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==5 & unlist(gregexpr(text=xChar, pattern='-'))[2]==8) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ-X-YY"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern='-')))==2 & unlist(gregexpr(text=xChar, pattern='-'))[1]==5 & unlist(gregexpr(text=xChar, pattern='-'))[2]==7) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "XX mmm ZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==3 & unlist(gregexpr(text=xChar, pattern=' '))[2]==7) {

			if (tolower(substr(xChar, start=4, stop=6))=='jan' |
				tolower(substr(xChar, start=4, stop=6))=='feb' |
				tolower(substr(xChar, start=4, stop=6))=='mar' |
				tolower(substr(xChar, start=4, stop=6))=='apr' |
				tolower(substr(xChar, start=4, stop=6))=='may' |
				tolower(substr(xChar, start=4, stop=6))=='jun' |
				tolower(substr(xChar, start=4, stop=6))=='jul' |
				tolower(substr(xChar, start=4, stop=6))=='aug' |
				tolower(substr(xChar, start=4, stop=6))=='sep' |
				tolower(substr(xChar, start=4, stop=6))=='oct' |
				tolower(substr(xChar, start=4, stop=6))=='nov' |
				tolower(substr(xChar, start=4, stop=6))=='dec' ) {

				years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=8, stop=9)))

			} else {

				years[i] <- NA

			}

		# if "XX mmm. ZZ"
		} else if (nchar(xChar)==10 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==3 & unlist(gregexpr(text=xChar, pattern=' '))[2]==8) {

			if (tolower(substr(xChar, start=4, stop=7))=='jan.' |
				tolower(substr(xChar, start=4, stop=7))=='feb.' |
				tolower(substr(xChar, start=4, stop=7))=='mar.' |
				tolower(substr(xChar, start=4, stop=7))=='apr.' |
				tolower(substr(xChar, start=4, stop=7))=='may.' |
				tolower(substr(xChar, start=4, stop=7))=='jun.' |
				tolower(substr(xChar, start=4, stop=7))=='jul.' |
				tolower(substr(xChar, start=4, stop=7))=='aug.' |
				tolower(substr(xChar, start=4, stop=7))=='sep.' |
				tolower(substr(xChar, start=4, stop=7))=='oct.' |
				tolower(substr(xChar, start=4, stop=7))=='nov.' |
				tolower(substr(xChar, start=4, stop=7))=='dec.' ) {

				years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=9, stop=10)))

			} else {

				years[i] <- NA

			}

		# if "X mmm ZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==2 & unlist(gregexpr(text=xChar, pattern=' '))[2]==6) {

			if (tolower(substr(xChar, start=3, stop=5))=='jan' |
				tolower(substr(xChar, start=3, stop=5))=='feb' |
				tolower(substr(xChar, start=3, stop=5))=='mar' |
				tolower(substr(xChar, start=3, stop=5))=='apr' |
				tolower(substr(xChar, start=3, stop=5))=='may' |
				tolower(substr(xChar, start=3, stop=5))=='jun' |
				tolower(substr(xChar, start=3, stop=5))=='jul' |
				tolower(substr(xChar, start=3, stop=5))=='aug' |
				tolower(substr(xChar, start=3, stop=5))=='sep' |
				tolower(substr(xChar, start=3, stop=5))=='oct' |
				tolower(substr(xChar, start=3, stop=5))=='nov' |
				tolower(substr(xChar, start=3, stop=5))=='dec' ) {

				years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=8)))

			} else {

				years[i] <- NA

			}

		# if "X mmm. ZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==2 & unlist(gregexpr(text=xChar, pattern=' '))[2]==7) {

			if (tolower(substr(xChar, start=3, stop=6))=='jan.' |
				tolower(substr(xChar, start=3, stop=6))=='feb.' |
				tolower(substr(xChar, start=3, stop=6))=='mar.' |
				tolower(substr(xChar, start=3, stop=6))=='apr.' |
				tolower(substr(xChar, start=3, stop=6))=='may.' |
				tolower(substr(xChar, start=3, stop=6))=='jun.' |
				tolower(substr(xChar, start=3, stop=6))=='jul.' |
				tolower(substr(xChar, start=3, stop=6))=='aug.' |
				tolower(substr(xChar, start=3, stop=6))=='sep.' |
				tolower(substr(xChar, start=3, stop=6))=='oct.' |
				tolower(substr(xChar, start=3, stop=6))=='nov.' |
				tolower(substr(xChar, start=3, stop=6))=='dec.' ) {

				years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=8, stop=9)))

			} else {

				years[i] <- NA

			}

		# if "XX YY ZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==3 & unlist(gregexpr(text=xChar, pattern=' '))[2]==6) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=8)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X Y ZZ"
		} else if (nchar(xChar)==6 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==2 & unlist(gregexpr(text=xChar, pattern=' '))[2]==4) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=5, stop=6)))

		# if "XX Y ZZ"
		} else if (nchar(xChar)==7 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==3 & unlist(gregexpr(text=xChar, pattern=' '))[2]==5) {

			years[i] <- if (yearLast) {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=7)))
			} else {
				9900 + suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=2)))
			}

		# if "X YY ZZ"
		} else if (nchar(xChar)==7 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==2 & unlist(gregexpr(text=xChar, pattern=' '))[2]==5) {

			years[i] <- 9900 + suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=7)))

		# if "XX YY ZZZZ"
		} else if (nchar(xChar)==10 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==3 & unlist(gregexpr(text=xChar, pattern=' '))[2]==6) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=7, stop=10)))

		# if "X Y ZZZZ"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==2 & unlist(gregexpr(text=xChar, pattern=' '))[2]==4) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=5, stop=8)))

		# if "XX Y ZZZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==3 & unlist(gregexpr(text=xChar, pattern=' '))[2]==5) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=9)))

		# if "X YY ZZZZ"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==2 & unlist(gregexpr(text=xChar, pattern=' '))[2]==5) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=6, stop=9)))

		# if "ZZZZ XX YY"
		} else if (nchar(xChar)==10 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==5 & unlist(gregexpr(text=xChar, pattern=' '))[2]==8) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ X Y"
		} else if (nchar(xChar)==8 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==5 & unlist(gregexpr(text=xChar, pattern=' '))[2]==7) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ XX Y"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==5 & unlist(gregexpr(text=xChar, pattern=' '))[2]==8) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if "ZZZZ X YY"
		} else if (nchar(xChar)==9 & length(unlist(gregexpr(text=xChar, pattern=' ')))==2 & unlist(gregexpr(text=xChar, pattern=' '))[1]==5 & unlist(gregexpr(text=xChar, pattern=' '))[2]==7) {

			years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=1, stop=4)))

		# if there is anywhere a number of 4 characters long
		} else if (nchar(xChar) > 4) {

			# cycle through each character and see if subsequent 3 characters make a year
			for (start in 1:(nchar(xChar)- 3)) {

				# if this string converted to a number makes sense
				if (!is.na(suppressWarnings(as.numeric(substr(x=xChar, start=start, stop=start + 3))))) {

					if (suppressWarnings(as.numeric(substr(x=xChar, start=start, stop=start + 3))) > 0 & nchar(suppressWarnings(as.numeric(substr(x=xChar, start=start, stop=start + 3))))==4) years[i] <- suppressWarnings(as.numeric(substr(x=xChar, start=start, stop=start + 3)))

				}

			}

		} else {

			years[i] <- NA

		}

	} # next date

	years <- suppressWarnings(as.integer(years))
	years[years == 0] <- NA
	years

}
