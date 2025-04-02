#' Close all open calls of sink()
#'
#' If you use the [sink()] function frequently, you can inadvertently open files while debugging. This function closes all open \code{sink} files.
#'
#' @param x Numeric: Number of sink files to try to close.
#' @param warn Logical: If \code{TRUE}, display warnings if attempts are made to close sink files that are not open.
#'
#' @returns Logical (invisibly).
#'
#' @examples
#' 
#' # create 3 sink files
#' for (i in 1:3) {
#'    tempFile <- tempfile(fileext = '.txt')
#'    sink(tempFile, split = TRUE)
#'    say('We will print this message on the screen and to a file.')
#' }
#' unsink(3) # close all 3
#' 
#' @aliases unsink
#' @rdname unsink
#' @export unsink
unsink <- function(x = 10, warn = TRUE) {
	if (warn) {
		for (i in 1:x) sink()
	} else {
		for (i in 1:x) suppressWarnings(sink())
	}
	invisible(TRUE)
}
