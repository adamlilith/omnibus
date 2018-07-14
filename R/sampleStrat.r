#' Stratified randomization
#'
#' This function scrambles values of a given column of a data frame in a stratified manner with respect to one or more other "covariate" columns. User can specify the other column(s) by which to scramble the target column and the starting width of the range (as a proportion of the total range of each covariate).
#' @param x Data frame containing at least two columns, one with numeric values and at least one more with numeric or factor values.
#' @param col Character or integer, name or number of column in \code{x} to swap values offset.
#' @param w Function or numeric value >0, sets window size of \emph{non-factor} covariates as a \emph{proportion} of their range. If using a function it must work on a list of values. It can be helpful if this function accepts the argument \code{'na.rm=T'} to avoid problems with \code{NA}s in the column specified by \code{col}. The default is the standard deviation divided by the range. This reduces the correlation between erstwhile perfectly correlated variables to ~0.80 (on average). Ignored for covariates that are factors.
#' @param d Numeric > 0, if no swappable value is found within \eqn{w * (max(col) - min(col))}, then \eqn{w} is expanded by \eqn{1 + d} iteratively until a value is found. Ignored for covariates that are factors.
#' @param by Character vector or integers. Name(s) or columns numbers of covariates by which to stratify the target column. Can also specify \code{'all'} (default) to stratify by all columns with a numeric/integer/factor class except the target column.
#' @param permuteBy Logical, if \code{TRUE} then in each step scramble the order of values in \code{by}. If \code{FALSE} then strata are considered for each covariate in teh order listed by \code{by}. This argument has no effect if \code{by} has just one value.
#' @details The script starts by randomly selecting a value \eqn{v_i} from the target column. It then finds the value of covariate \eqn{c_j}, that is associated with \eqn{v_i}. Call the particular value of \eqn{c_j} associated with \eqn{v_i} \eqn{c_j:i}. If \eqn{c_j} is a continuous variable it then finds all values \eqn{c_{v}} that fall within \eqn{[c_j:i - w * max(c_j) - min(c_j), c_j:i + w * max(c_j) - min(c_j)]} where \eqn{w} is a constant indicating the proportion of the range of \eqn{c_j} that is to be considered. It then randomly selects a value of \eqn{v_k} from those associated with this range of \eqn{c_j} and swaps \eqn{v_i} with this value (note that \eqn{v_i} can = \eqn{v_k} and in fact be the same value). If no values of \eqn{c_j} other than the one associated with \eqn{v_i} are found within this range, then the window is expanded iteratively by a factor of \eqn{w * (1 + d)} until at least one more values that have yet to be swapped have been found. The procedure then finds a window around \eqn{v_k} as described above (or randomly selects a new \eqn{v_i} if \eqn{v_i} was \eqn{v_k}) and continues. If there is an odd number of values then the last value is kept as is (not scrambled). If \eqn{c_j} is a categorical variable (a factor), then the script finds all values of of \eqn{v} in same factor level as \eqn{v_i}. Swaps of \eqn{v} occur within this level of \eqn{c_j}. However, if there is <2 of these (including the value associated with \eqn{v_i}), then the script looks to the next factor level. The "next" is taken to be the factor level with the least difference between \eqn{v_i} and the average of values of \eqn{v} associated with the potential "next" factor level. The "window" for a factor level is thus the level plus one or more levels with the closest average values of \eqn{v} given that there is >1 value of \eqn{v} within this group that has yet to be swapped. If there is more than one covariate, then these steps are repeated for each covariate (i.e., selecting values of \eqn{v} given the stratum identified in covariate \eqn{c_j}, then among these values those also in the stratum identified n covariate \eqn{c_k}, and so on. In this case the order in which the covariates are listed in \code{by} can affect the outcome; the order can be permuted each values of \eqn{v_i} if \code{permuteBy} is \code{TRUE}. Note: In the script documentation, a target value and the associated values of its covariates are called "coordinates".
#' @return A data frame with one column swapped in a stratified manner relative another column or set of columns.
#' @seealso \code{\link[base]{sample}}
#' @examples
#' # Example #1: Scramble column 1 with respect to columns 2 and 3.
#' # Note in the output high values of "a" tend to be associated with
#' # high values of "b" and low values of "c". This tendency decreases as "w" increases.
#'
#' x <- data.frame(a=1:20, b=1:20, c=20:1, d=c(rep('a', 10), rep('b', 10)))
#' x
#'
#' # scramble by all other columns
#' sampleStrat(x=x, col=1, w=0.2, by='all', d=0.1)
#'
#' # scramble by column "d"
#' sampleStrat(x=x, col=1, w=0.2, by='d', d=0.1)
#'
#' # Example #2: The target variable and covariate are equal
#' # (perfectly collinear). How wide must the window (set by
#' # argument "w'" be to reduce the average correlation
#' # between them to an arbitrary low level?
#'
#' df=data.frame(a=1:100, b=1:100)
#' cor(df) #' perfect correlation
#'
#' corFrame <- data.frame()
#' for (thisW in seq(0.1, 1, 0.1)) {
#'     for (countRep in 1:10) {
#'         df2 <- sampleStrat(x=df, col=1, w=thisW)
#'        corFrame <- rbind(corFrame, data.frame(w=thisW, cor=cor(df2)[1, 2]))
#'     }
#' }
#'
#' boxplot(cor ~ w, data=corFrame, xlab='w', ylab='correlation coefficient')
#' @export
sampleStrat <- function(
	x,
	col,
	w = function(x) stats::sd(x, na.rm=T) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)),
	d = 0.1,
	by = 'all',
	permuteBy = TRUE
) {

	# function to return range
	rrange <- function(x) diff(range(x, na.rm=TRUE))

	if (d <= 0) {

		warning('Argument d must be >0. Setting to default value of 0.1', immediate.=T)
		d <- 0.1
	}

	# get index position of target column
	if (class(col)=='character') col <- which(names(x) == col)

	# get column index position(s) of covariate column(s)
	if (class(by) == 'character' && by == 'all') {

		by <- if (class(col) %in% 'numeric' | class(col) %in% 'integer') {
			(1:ncol(x))[-col]
		} else {
			(1:ncol(x))[-which(names(x) %in% col)]
		}

	} else {

		by <- if (class(by) == 'numeric' | class(by) == 'integer') {
			by
		} else {
			by <- which(names(x) %in% by)
		}

	}

	# vector to hold swapped values
	swappedValues <- rep(NaN, nrow(x))

	# vector indicating which rows can be swapped
	canSwapFrom <- canSwapTo <- rep(TRUE, nrow(x))

	# while there is at least one value that can be swapped, swap!
	while (sum(canSwapFrom) > 1) {

		## randomly select a value of the target column
		targetIndex <- sample(which(canSwapFrom), 1)

		## thisCanSwapTo becomes unique to these coordinates
		thisCanSwapTo <- canSwapTo

		if (permuteBy) by <- sample(by, length(by))

		## for each covariate, pare the data frame to just the rows within its window... if there is just one row (the row to be swapped), then expand the window for this variable until there is >1
		for (thisCov in by) {

			# if covariate is NOT a FACTOR
			if (class(x[ , thisCov]) != 'factor') {

				# calculate window
				window <- if (class(w)=='function') { w(x[ , thisCov]) } else { w }

				# make window larger if there is just 1 coordinate in the window (ie the coordinate to be swapped)
				while (sum(thisCanSwapTo * (x[ , thisCov] >= x[targetIndex, thisCov] - window * rrange(x[ , thisCov]) & x[ , thisCov] <= x[targetIndex, thisCov] + window * rrange(x[ , thisCov]))) <= 1) {

					window <- window * (1 + d)

				}

				# note row indices of values that can be swapped in this swap
				thisCanSwapTo <- as.logical(thisCanSwapTo * (x[ , thisCov] >= x[targetIndex, thisCov] - window * rrange(x[ , thisCov]) & x[ , thisCov] <= x[targetIndex, thisCov] + window * rrange(x[ , thisCov])))

			# if covariate is a FACTOR
			} else {

				# get the factor name (the starting window)
				window <- as.character(x[targetIndex, thisCov])

				# if there are no swappable values in this window, add factor level with least difference between target value v_i and mean value of target variable v for the any given factor level
				while (sum(thisCanSwapTo * x[ , thisCov] %in% window) <= 1) {

					window <- c(window, names(which.min(abs(x[targetIndex, col] - tapply(x[!(x[ , thisCov] %in% window), col], x[which(!(x[ , thisCov] %in% window)), thisCov], function(x) mean(x, na.rm=T))))))

				}

				# note row indices of values that can be swapped in this swap
				thisCanSwapTo <- as.logical(thisCanSwapTo * x[ , thisCov] %in% window)

			} # if covariate is a factor

		} # for each covariate pare choices

		# get row index of replaced value (*can* be target coordinate index)
		replacedRow <- sample((1:nrow(x))[thisCanSwapTo], 1)

		# put target value into its slot in swap vector
		swappedValues[replacedRow] <- x[targetIndex, col]

		# remove row from which values were swapped as a future candidate for a swap

		canSwapFrom[targetIndex] <- canSwapTo[replacedRow] <- FALSE

	} # while there is > one value that can be swapped

	# swap last value
	if (any(canSwapTo)) swappedValues[canSwapTo] <- x[canSwapFrom, col]

	# replace target column with swapped values
	x[ , col] <- swappedValues

	x

}
