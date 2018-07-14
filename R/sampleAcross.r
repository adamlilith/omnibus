#' Permute values across two vectors or columns in two data frames or matrices
#' 
#' This function permutes values across two or more vectors or columns across two or more data frames or matrices. If vectors then all values are swapped randomly and the output is a list object with vectors of the same length. If data frames or matrices then values in selected columns are swapped across the data frames or matrices and the output is a list object with data frames or matrices of the same dimension as the originals.
#' @param ... One or more vectors, data frames, or matrices (all objects must be the same class).
#' @param replace Logical. If \code{TRUE} then sample with replacement. If \code{FALSE} (default) then sample without replacement.
#' @param by Character list or list of integers. Names of columns or column numbers to permute (only used if \code{...} is data frames or matrices). If left as \code{NULL} (default) the all columns are permuted.
#' @return A list object with same number of elements as in \code{...} with the original dimensions. The order is the same as in \code{...} (e.g., so if the call is like \code{sampleAcross(a, b, c)} then the output will be a list with permuted versions of \code{a}, \code{b}, and \code{c} in that order).
#' @seealso [base::sample()]
#' @examples
#' x1 <- 1:5
#' x2 <- 6:10
#' x3 <- 50:60
#' sampleAcross(x1, x2, x3)
#' sampleAcross(x1, x2, x3, replace=TRUE)
#'
#' a <- data.frame(x=1:10, y=letters[1:10])
#' b <- data.frame(x=11:20, y=letters[11:20])
#' sampleAcross(a, b, by='y')
#' sampleAcross(a, b)

# combine two data frames, permute along one or more variables, and re-replit into input data frames with same number of original rows
sampleAcross <- function(..., by=NULL, replace=FALSE) {

	# input/output
	input <- list(...)
	out <- list()

	# if data frames or matrices permute columns
	if (class(input[[1]]) %in% c('data.frame', 'maxtrix')) {
		
		# combine
		x <- input[[1]]
		if (length(input) > 1) {
			for (i in 2:length(input)) {
				x <- rbind(x, input[[i]])
			}
		}
		
		# permute
		if (is.null(by)) by <- 1:ncol(x)
		
		for (thisBy in by) {
			x[ , thisBy] <- sample(x[ , thisBy], replace=replace)
		}

		# split
		for (i in 1:length(input)) {
			
			start <- 1 + sum(sapply(input, nrow)[1:i]) - nrow(input[[i]])
			end <- sum(sapply(input, nrow)[1:i])
			
			out[[i]] <- x[start:end, ]
			
		}
		
	# if vectors computer values
	} else {

		# combine
		x <- input[[1]]
		
		if (length(input) > 1) {
			for (i in 2:length(input)) {
				x <- c(x, input[[i]])
			}
		}
		
		# permute
		x <- sample(x, replace=replace)
		
		# split
		for (i in 1:length(input)) {
			
			start <- 1 + sum(sapply(input, length)[1:i]) - length(input[[i]])
			end <- sum(sapply(input, length)[1:i])
			
			out[[i]] <- x[start:end]
			
		}
	
	}
	
	# name elements of output same as names of input
	# called <- as.list(substitute(list(...)))[-1L]
	# inNames <- character()
	# for (i in seq_along(input)) inNames <- c(inNames, as.character(called[[i]]))
	inNames <- ellipseNames(...)
	if (length(inNames) == length(input)) names(out) <- inNames
	
	out

}