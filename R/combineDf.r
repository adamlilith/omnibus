#' Combine data frames with different fields using a crosswalk table
#'
#' This function combines multiple data frames, possibly with different column names, into a single data frame.  Usually \code{\link[base]{merge}} will be faster and easier to implement if the columns to be merged on have the same names, and \code{\link{rbind}} will always be faster and much easier if the column names match exactly.
#' @param ... A list of data frames. If ignored, then paths and file names of data frames can be specified in \code{crosswalk}.
#' @param crosswalk Data frame. Column names are fields desired in the output data frame. Each row corresponds to a different data frame to join. If \code{...} is not used then the first column \emph{must} have the paths and file names to CSV, RDS, or RData files representing data frames to join. Other than this column, the elements of each cell contain the name of the column in each data frame that coincides with the column name in the \code{crosswalk} table.  For example, if the final output is to have a column by the name of "species" and "data frame #1" has a column named "Species" and "data frame #2" has a column named "scientificName", then the first value in \code{crosswalk} under its "species" column will be "Species" and the second "scientificName". More complex joining can be done using the following in cells of \code{crosswalk}:
#' \itemize{
#' \item \code{_} at start of value: indicates the value in the \code{crosswalk} table will be read as text and repeated in the output in each row (minus the initial "_"). For example, "_inspected" will repeat the string "inspected" in every row of the output corresponding to the respective data frame.
#' \item \code{'c(~~~)'}: This will paste together fields in source data frame named in ... using the string specified in \code{sep} ("~~~" represents names of the respective data frame). Note that the entire string must be inside a single or double quotes as in \code{'c()'} or \code{"c()"} and the columns named inside \code{c()} must be delineated by the other kind of quote (single if \code{c()} is delineated by double, and vice versa).
#' \item \code{NA}: Repeats \code{NA}.
#' }
#' @param classes Character or character list, specifies the classes (e.g., numeric, character) to be assigned to each column in the output table. If NULL, all classes will be assumed to be character.  If just one value is listed, all columns will be set to this class. If a list, it must be the same length as the number of columns in \code{crosswalk} and specify the class of each column.
#' @param use Logical, Character, or NULL, if \code{...} is used then this is a list of logical elements (\code{TRUE} or \code{FALSE}), \emph{or} a column name of \code{crosswalk} with logical values indicating whether or not this particular data frame is to be collated, \code{or} \code{NULL}, in which case all data frames are used (default).
#' @param sep Character, specifies the string to put between fields combined with the \code{c(~~~~)} format in \code{crosswalk}.
#' @param verbose Logical, if \code{TRUE} prints extra information during execution.  Useful for debugging the \code{crosswalk} table.
#' @return A data frame.
#' @seealso \code{\link[base]{merge}}, \code{\link{rbind}}
#' @examples
#' df1 <- data.frame(x1=1:5, x2='valid', x3=letters[1:5], x4=LETTERS[1:5], x5='stuff')
#' df2 <- data.frame(y1=11:15, y3=rev(letters)[1:5])
#'
#' crosswalk <- data.frame(
#'   a=c('x1', 'y1'),
#'   b=c('x2', '_valid'),
#'   c=c('c("x3", "x4")', 'y3'),
#'   d=c('x5', NA)
#' )
#'
#' out <- combineDf(df1, df2, crosswalk=crosswalk)
#' out
#' @export
combineDf <- function(
	...,
	crosswalk,
	sep='; ',
	use=NULL,
	classes=NULL,
	verbose=FALSE
) {

	if (verbose) omnibus::say('Combining data frames...')

	dots <- list(...)
	if (length(dots) != 0 && length(dots) != nrow(crosswalk)) stop('Number of data frames does not match number of rows in "crosswalk".')

	dfSource <- if (length(dots) > 0) { 'dots' } else { 'crosswalk' }

	# specify column classes
	if (is.null(classes)) classes <- rep('character', ncol(crosswalk))
	if (length(classes)==1) classes <- rep(classes, ncol(crosswalk))

	use <- if (is.null(use)) { rep(TRUE, nrow(crosswalk)) } else { crosswalk[ , use] }

	out <- data.frame()

	# for each source frame
	for (countDf in 1:nrow(crosswalk)) {

		if (verbose) omnibus::say('  Data frame ', countDf, ifelse(dfSource == 'crosswalk', paste0(' from: ', as.character(crosswalk[countDf, 1])), ''))

		# get data frame
		if (dfSource == 'dots') {

			sourceFrame <- dots[[countDf]]

		} else if (!is.na(crosswalk[countDf, 1])) {

			fileName <- as.character(crosswalk[countDf, 1])
			if (tolower(substr(fileName, nchar(fileName) - 4, nchar(fileName))) == '.csv') {
				utils::read.csv(fileName)
			} else if (tolower(substr(fileName, nchar(fileName) - 4, nchar(fileName))) == '.rds') {
				readRDS(fileName)
			} else if (tolower(substr(fileName, nchar(fileName) - 6, nchar(fileName))) == '.Rdata') {
				load(fileName)
			} else {
				stop(paste0('Cannot load data frame #', countDf))
			}

		}

		# populate each column in new data frame
		if (dfSource == 'crosswalk') {
			newFrame <- data.frame(x=rep(as.character(crosswalk[countDf, 1]), nrow(sourceFrame)))
		} else {

			countCol <- 1

			# add NA values
			if (is.na(crosswalk[countDf, countCol])) {

				newFrame <- data.frame(x=rep(NA, nrow(sourceFrame)))

			# repeat values
			} else if (substr(x=as.character(crosswalk[countDf, countCol]), start=1, stop=1)=='_') {

				newFrame <- data.frame(x=rep(substr(x=as.character(crosswalk[countDf, countCol]), start=2, stop=nchar(as.character(crosswalk[countDf, countCol]))), nrow(sourceFrame)))

			# concatenate values
			} else if (substr(x=as.character(crosswalk[countDf, countCol]), start=1, stop=2)=='c(') {

					# get names of fields to paste
					fieldsToPaste <- eval(parse(text=as.character(crosswalk[countDf, countCol])))

					# add first source field
					newField <- as.character(sourceFrame[ , fieldsToPaste[1]])

					# add each additional source field
					for (countVal in 2:length(fieldsToPaste)) {

						newField <- paste(newField, as.character(sourceFrame[ , fieldsToPaste[countVal]]), sep=sep)

					}

				newFrame <- data.frame(x=newField)

			# add field values from source table
			} else {

				newFrame <- data.frame(x=sourceFrame[ , as.character(crosswalk[countDf, countCol])])

			}

		}

		names(newFrame)[1] <- names(crosswalk)[1]

		# populate each subsequent row in new data frame
		for (countCol in 2:ncol(crosswalk)) {

			if (verbose) say('    ', countCol, ' | crosswalk field: ', names(crosswalk[countCol]), ' | source field(s): ', as.character(crosswalk[countDf, countCol]))

			# add NA values
			if (is.na(crosswalk[countDf, countCol])) {

				newFrame <- cbind(newFrame, x=rep(NA, nrow(sourceFrame)))

			# repeat values
			} else if (substr(x=as.character(crosswalk[countDf, countCol]), start=1, stop=1)=='_') {

				newFrame <- cbind(newFrame, x=rep(substr(x=as.character(crosswalk[countDf, countCol]), start=2, stop=nchar(as.character(crosswalk[countDf, countCol]))), nrow(sourceFrame)))

			# concatenate values
			} else if (substr(x=as.character(crosswalk[countDf, countCol]), start=1, stop=2)=='c(') {

					# get names of fields to paste
					fieldsToPaste <- eval( parse( text=as.character(crosswalk[countDf, countCol]) ) )

					newField <- as.character( sourceFrame[ , fieldsToPaste[1]] ) # add first source field

					for (countVal in 2:length(fieldsToPaste)) { # add each additional source field

						newField <- paste( newField, as.character(sourceFrame[ , fieldsToPaste[countVal]]), sep=sep )

					}

				newFrame <- cbind(newFrame, data.frame(x=newField))

			# add field values from source table
			} else {

				newFrame <- cbind(newFrame, data.frame(x=sourceFrame[ , as.character(crosswalk[countDf, countCol])]))

			}

			# assign class
			newFrame$x <- if (classes[countCol]=='character') {
				as.character(newFrame$x)
			} else if (classes[countCol]=='logical') {
				as.logical(newFrame$x)
			} else if (classes[countCol]=='integer') {
				as.integer(newFrame$x)
			} else {
				as.numeric(newFrame$x)
			}

			# name new field
			names(newFrame)[ncol(newFrame)] <- names(crosswalk)[countCol]

		} # next column in output frame

		# combine with output
		out <- rbind(out, newFrame)

	}

	rownames(out) <- 1:nrow(out)

	if (verbose) say('Done!')
	out

}
