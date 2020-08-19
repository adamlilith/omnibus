#' Erase everything in memory and perform garbage collection.
#'
#' This is a one-line function for the code: \code{rm(list=ls()); gc()}.
#' @param rm Logical, if \code{FALSE} (default), everything will be cleared from memory. If \code{TRUE}, then nothing will be cleared but garbage collection will be implemented.
#' @return Nothing (clears everything from memory).
#' @examples
#' clear(FALSE) # will not clear memory
#' @export

clear <- function(rm = FALSE) {
	if (rm) rm(list=ls())
	gc()
}
