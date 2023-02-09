#' Combine data frames with different fields using a crosswalk table
#'
#' @description This function combines multiple "source" data frames, possibly with different column names, into a single "destination" data frame.  Usually \code{\link[base]{merge}} will be faster and easier to implement if the columns to be merged on have the same names, and \code{\link{rbind}} will always be faster and much easier if the column names and data types match exactly.\cr
#'
#' The key tool in this function is a "crosswalk" table (a \code{data.frame}) that tells the function which fields in each source data frame match to the final fields in the destination data frame. Values in source data frame fields can be used as-is, combined across fields, or have functions applied to them before they are put into the destination data frame. If a source data frame doe snot gave a field that matches the destination field, a default value (including \code{NA}) can be assigned to all cells for that source data frame.\cr
#'
#' The data frames to be combined can be provided in \code{...} or as file names in the first column of the crosswalk table. These can be either CSV files (extension "\code{.csv}"), TAB files (extension "\code{.tab}"), "Rdata" files (read using \code{\link{load}} and with a "\code{.rda}" or "\code{.rdata}" extension), or "RDS" files (read using \code{\link{readRDS}} and with a "\code{.rds}" extension). The file type will be intuited from the extension, and its case does not matter. Note that if an object in an Rdata file has the same name as an object in this function (i.e., any of the arguments plus any objects internal to the function), this may cause a conflict. To help obviate this issue, all internal objects are named with a period at the end (e.g., "\code{crossCell.}" and "\code{countDf.}").\cr
#'
#' All cells in each source data frame will have leading and trailing white spaces removed before combining.
#'
#' @param ... Data frames to combine. These \emph{must} be listed in the order that they appear in the \code{crosswalk} table.
#'
#' @param crosswalk A \code{data.frame}. Column names are fields in the destination data frame. Each row corresponds to a different data frame to join. If \code{...} is not used then the first column \emph{must} have the paths and file names to CSV, TAB, Rdata, or RDS files representing data frames to join. All objects will be coerced to \code{data.frame}s.\cr
#'
#' Other than this column, the elements of each cell contain the name of the column in each source data frame that coincides with the column name in the \code{crosswalk} table.  For example, suppose the destination data frame is to have a column by the name of "species" with names of species within it. If the first source data frame has a column named "Species" and the second source data frame has a column named "scientificName", then the first value in \code{crosswalk} under its "species" column will be "Species" and the second "scientificName". This will take all the values in the "Species" field of the first source data frame and all the values in the "scientificName" field in the second source data frame and put them both into the "species" field of the destination frame.\cr
#'
#' More complex operations can be done using the following in cells of \code{crosswalk}:
#' \itemize{
#'	\item Filling all cells with a single value: If the value in the crosswalk cell begins with "\code{\%fill\%}", then the value that follows it will be repeated in destination data frame in each row. For example,\cr
#'
#'	\code{\%fill\% inspected}\cr
#'
#'	will repeat the string "inspected" in every row of the output corresponding to the respective source data frame (any spaces immediately after \code{\%fill\%} are ignored).
#'	\item Concatenating (pasting) columns together: To combine multiple fields, begin crosswalk cell with "\code{\%cat\%}", then list the fields to combine (with or without commas separating them). For example, if the crosswalk cell has\cr
#'
#' \code{\%cat\% field1, field2, field3}\cr
#'
#' then the resulting column in the destination data frame will have values from \code{field1}, \code{field2}, and \code{field3} pasted together. See also argument \code{collapse}.
#'	\item Applying a function: You can manipulate values by including functions in the crosswalk cell. The crosswalk cell should begin with "\code{\%fun\%}", then be followed an expression to evaluate. Expressions should generally \emph{not} use the "\code{<-}" operator (or the equals operator used in the same way).  For example:\cr
#'
#'	\code{\%fun\% ifelse(as.numeric(field1) >20, NA, as.numeric(field1))}\cr
#'
#' will create a column that is \code{NA} whenever values in \code{field1} are >20, and the value in \code{field1} otherwise. Note that for mathematical operations, it is almost always necessary to use \code{\link{as.numeric}} around column names representing numbers since fields are read in as characters.
#' }
#'
#' @param classes Character or character list, specifies the classes (e.g., character, logical, numeric, integer) to be assigned to each column in the output table. If \code{NULL}, all classes will be assumed to be character.  If just one value is listed, all columns will be set to this class. If a list, it must be the same length as the number of columns in \code{crosswalk} and specify the class of each column. If it has names, then the names must correspond to the columns in \code{crosswalk} and will be used to assign the data type to the corresponding columns.
#'
#' @param useColumns,excludeColumns Logical, character vector, integer vector, or \code{NULL}. Indicates which columns in the crosswalk table are to be used or not to be used. These can be given as a \code{TRUE}/\code{FALSE} vector, a vector of column names, or a vector of column index values (integers). You can only specify \code{useColumns} or \code{excludeColumns} at a time (one or both must be \code{NULL}).  If both are \code{NULL} (default), then all columns in the crosswalk will be used.
#'
#' @param useFrames Logical, character, or \code{NULL}.  Indicates if a particular source data frame should be used.  This should be a \code{TRUE}/\code{FALSE} vector or the name of a column in the crosswalk table that has \code{TRUE}/\code{FALSE} values. If this is the name of a column, the column will be removed from the columns in \code{useColumns}. If \code{NULL} (default). then all data frames in the crosswalk will be used.
#'
#' @param collapse Character, specifies the string to put between fields combined with the \code{\%cat\%} operator in the \code{crosswalk} table.
#'
#' @param verbose Logical, if \code{TRUE} prints extra information during execution.  Useful for debugging the \code{crosswalk} table.
#'
#' @return A data frame.
#'
#' @seealso \code{\link[base]{merge}}, \code{\link{rbind}}
#'
#' @examples
#' df1 <- data.frame(x1=1:5, x2=FALSE, x3=letters[1:5], x4=LETTERS[1:5],
#'        x5='stuff', x6=11:15)
#' df2 <- data.frame(y1=11:15, y2=rev(letters)[1:5], y3=runif(5))
#'
#' crosswalk <- data.frame(
#'   a = c('x1', 'y1'),
#'   b = c('x2', '%fill% TRUE'),
#'   c = c('%cat% x3 x4', 'y2'),
#'   d = c('x5', '%fill% NA'),
#'	 e = c('%fun% as.numeric(x6) > 12', '%fun% round(as.numeric(y3), 2)')
#' )
#'
#' combined <- combineDf(df1, df2, crosswalk=crosswalk)
#' combined
#'
#' @export
combineDf <- function(
	...,
	crosswalk,
	collapse = '; ',
	useColumns = NULL,
	excludeColumns = NULL,
	useFrames = NULL,
	classes = NULL,
	verbose = FALSE
) {

	if (verbose) say('Combining data frames...')

	dots. <- list(...)
	dfSource. <- if (length(dots.) > 0L) { 'dots.' } else { 'crosswalk' }

	# which data frames to use?
	crosswalk <- as.data.frame(crosswalk)
	if (is.null(useFrames)) {
		useFrames <- rep(TRUE, nrow(crosswalk))
		excludeUseFrame. <- FALSE
	} else if (inherits(useFrames, 'character') && length(useFrames) == 1L) {
		useFramesName. <- useFrames
		useFrames <- crosswalk[ , useFrames, drop=TRUE]
		excludeUseFrame. <- TRUE
	} else {
		stop('Invalid value for "useFrames" argument.')
	}
	crosswalk <- crosswalk[useFrames, ]
	dots. <- dots.[which(useFrames)]

	# get data frames
	if (length(dots.) > 0L && length(dots.) != sum(useFrames)) stop('Number of data frames does not match number of TRUE cases in argument "useFrames."')

	# which columns of crosswalk to use?
	if (!is.null(useColumns) & !is.null(excludeColumns)) stop('You cannot use "useColumns" and "excludeColumns" at the same time.\n  At least one must be NULL.')
	
	if (!is.null(useColumns)) {
		if (any(!useColumns)) crosswalk <- crosswalk[ , useColumns]
	} else if (!is.null(excludeColumns)) {
		if (inherits(excludeColumns, c('logical', 'numeric', 'integer'))) excludeColumns <- names(crosswalk)[excludeColumns]
		crosswalk <- crosswalk[ , names(crosswalk) %notin% excludeColumns]
	}

	if (excludeUseFrame. && any(names(crosswalk) %in% useFramesName.)) {
		crosswalk <- crosswalk[ , names(crosswalk) %notin% useFramesName.]
	}


	# specify column classes
	if (is.null(classes)) classes <- rep('character', ncol(crosswalk))
	if (length(classes) == 1L) classes <- rep(classes, ncol(crosswalk))
	if (is.null(names(classes)) && length(classes) != ncol(crosswalk)) stop('The length of "classes" is different from the number of columns in "crosswalk."')

	out. <- data.frame()

	# for each source frame
	for (countDf. in 1L:nrow(crosswalk)) {

		if (verbose) say('  Data frame ', countDf., ifelse(dfSource. == 'crosswalk', paste0(' from: ', as.character(crosswalk[countDf., 1])), ''))

		# get data frame
		if (dfSource. == 'dots.') {

			sourceFrame. <- dots.[[countDf.]]

		} else if (!is.na(crosswalk[countDf., 1L])) {

			fileName. <- as.character(crosswalk[countDf., 1L])
			if (tolower(substr(fileName., nchar(fileName.) - 4L, nchar(fileName.))) == '.csv') {
				sourceFrame. <- utils::read.csv(fileName.)
			} else if (tolower(substr(fileName., nchar(fileName.) - 4L, nchar(fileName.))) == '.tab') {
				sourceFrame. <- utils::read.csv(fileName., collapse='\t')
			} else if (tolower(substr(fileName., nchar(fileName.) - 4L, nchar(fileName.))) == '.rds') {
				sourceFrame. <- readRDS(fileName.)
			} else if (tolower(substr(fileName., nchar(fileName.) - 6L, nchar(fileName.))) %in% c('.rdata', '.rda')) {
				objects1. <- ls()
				load(fileName.)
				objects2. <- ls()
				newObject. <- objects2.[objects2. %notin% objects1.]
				sourceFrame. <- get(newObject., inherits=FALSE)
				rm(newObject.)
			} else {
				stop(paste0('Cannot load data frame #', countDf.))
			}

		}

		sourceFrame. <- as.data.frame(sourceFrame.)
		for (i in 1L:ncol(sourceFrame.)) sourceFrame.[ , i] <- trimws(sourceFrame.[ , i])

		### populate FIRST column in new data frame
		if (dfSource. == 'crosswalk') {
			# if file names
			newFrame. <- data.frame(x.=rep(as.character(crosswalk[countDf., 1L]), nrow(sourceFrame.)))
		} else {

			countCol. <- 1L

			crossCell. <- crosswalk[countDf., countCol.]
			crossCell. <- as.character(crossCell.)

			# add NA values
			if (crossCell. == 'NA' | crossCell. == '') {

				newField. <- data.frame(x.=rep(NA, nrow(sourceFrame.)))

			# fill values
			} else if (substr(crossCell., start=1L, stop=6L) == '%fill%') {

				crossCell. <- substr(crossCell., 7L, nchar(crossCell.))
				crossCell. <- trimws(crossCell.)
				newField. <- data.frame(x.=rep(crossCell., nrow(sourceFrame.)))

			# concatenate values
			} else if (substr(crossCell., start=1L, stop=5L) == '%cat%') {

				crossCell. <- substr(crossCell., 6L, nchar(crossCell.))
				crossCell. <- trimws(crossCell.)
				crossCell. <- gsub(crossCell., pattern=',', replacement=' ')
				crossCell. <- strsplit(crossCell., split=' ')[[1L]]
				if (any(crossCell. == '')) crossCell. <- crossCell.[crossCell. != '']
				
				nots. <- any(crossCell. %notin% names(sourceFrame.))
				if (any(nots.)) stop('Cannot find column(s) in the source frame:', paste(crossCell.[nots.], collapse=' '))

				newField. <- apply(sourceFrame.[ , crossCell.], 1L, paste, collapse=collapse)
				newField. <- data.frame(x.=newField.)

			# execute function
			} else if (substr(crossCell., start=1L, stop=5L) == '%fun%') {

				# clean functions
				crossCell. <- substr(crossCell., 6L, nchar(crossCell.))
				crossCell. <- trimws(crossCell.)
				firstIsEquals. <- (substr(crossCell., 1L, 1L) == '=') & (substr(crossCell., 2L, 2L) != '=')
				if (any(firstIsEquals.)) {
					crossCell.[firstIsEquals.] <- substr(crossCell., 2L, nchar(crossCell.[firstIsEquals.]))
					crossCell. <- trimws(crossCell.)
				}

				express. <- paste0('with(sourceFrame., ' ,  crossCell., ')')
				newField. <- data.frame(x.=eval(parse(text=express.)))

			# add field values from source table
			} else {
				if (crossCell. %notin% names(sourceFrame.)) stop('Cannot find this column in the source frame:', crossCell.)
				newField. <- data.frame(x.=sourceFrame.[ , crossCell.])
			}

		} # first column

		names(newField.)[1L] <- names(crosswalk)[1L]
		newFrame. <- newField.

		### populate each subsequent row in new data frame
		##################################################
		for (countCol. in 2L:ncol(crosswalk)) {

			crossCell. <- crosswalk[countDf., countCol.]
			crossCell. <- as.character(crossCell.)

			if (verbose) say('    ', countCol., ' | crosswalk field: ', names(crosswalk[countCol.]), ' | source field(s): ', crossCell.)

			# add NA values
			if (crossCell. == 'NA' | crossCell. == '') {

				newField. <- data.frame(x.=rep(NA, nrow(sourceFrame.)))

			# fill values
			} else if (substr(crossCell., start=1L, stop=6L) == '%fill%') {

				crossCell. <- substr(crossCell., 7L, nchar(crossCell.))
				crossCell. <- trimws(crossCell.)
				newField. <- data.frame(x.=rep(crossCell., nrow(sourceFrame.)))

			# concatenate values
			} else if (substr(crossCell., start=1L, stop=5L) == '%cat%') {

				crossCell. <- substr(crossCell., 6L, nchar(crossCell.))
				crossCell. <- trimws(crossCell.)
				crossCell. <- gsub(crossCell., pattern=',', replacement=' ')
				crossCell. <- strsplit(crossCell., split=' ')[[1L]]
				if (any(crossCell. == '')) crossCell. <- crossCell.[crossCell. != '']

				nots. <- any(crossCell. %notin% names(sourceFrame.))
				if (any(nots.)) stop('Cannot find column(s) in the source frame: ', paste(crossCell.[nots.], collapse=' '))

				newField. <- apply(sourceFrame.[ , crossCell.], 1L, paste, collapse=collapse)
				newField. <- data.frame(x.=newField.)

			# execute function
			} else if (substr(crossCell., start=1L, stop=5L) == '%fun%') {

				# clean functions
				crossCell. <- substr(crossCell., 6L, nchar(crossCell.))
				crossCell. <- trimws(crossCell.)
				firstIsEquals. <- (substr(crossCell., 1L, 1L) == '=') & (substr(crossCell., 2L, 2L) != '=')
				if (any(firstIsEquals.)) {
					crossCell.[firstIsEquals.] <- substr(crossCell., 2L, nchar(crossCell.[firstIsEquals.]))
					crossCell. <- trimws(crossCell.)
				}

				express. <- paste0('with(sourceFrame., ' ,  crossCell., ')')
				newField. <- data.frame(x.=eval(parse(text=express.)))

			# add field values from source table
			} else {
				if (crossCell. %notin% names(sourceFrame.)) stop('Cannot find this column in the source frame: ', crossCell.)
				newField. <- data.frame(x.=sourceFrame.[ , crossCell., drop=TRUE])
			}

			newFrame. <- cbind(newFrame., newField.)

			# name new field
			names(newFrame.)[ncol(newFrame.)] <- names(crosswalk)[countCol.]

		} # next column in output frame

		# combine with output
		out. <- rbind(out., newFrame., make.row.names=FALSE)

	}

	# assign classes
	for (countCol. in 1L:ncol(crosswalk)) {
		
		if (!is.null(names(classes))) {
		
			datatype. <- classes[names(classes) %in% names(crosswalk)[countCol.]]
			ex. <- paste0('as.', datatype., '(as.character(out.[ , ', countCol., ']))')
			out.[ , countCol.] <- eval(str2expression(ex.))
			
		} else if (classes[countCol.]=='character') {
			out.[ , countCol.] <- as.character(out.[ , countCol.])
		} else if (classes[countCol.]=='logical') {
			out.[ , countCol.] <- as.logical(out.[ , countCol.])
		} else if (classes[countCol.]=='integer') {
			out.[ , countCol.] <- as.integer(out.[ , countCol.])
		} else {
			out.[ , countCol.] <- as.numeric(out.[ , countCol.])
		}
		
	}

	if (verbose) say('Done!')
	out.

}
