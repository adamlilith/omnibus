#' Replacement for \code{list.files()}
#'
#' This function is a slightly friendlier version of \code{\link[base]{list.files}} in that it automatically includes the \code{full.names=TRUE} argument.
#' @param x Path name of folder containing files to list.
#' @param ... Arguments to pass to \code{list.files} (other than \code{full.names}).
#' @return Character list.
#' @seealso \code{\link[base]{list.files}}
#' @examples
#' # list files in location where R is installed
#' listFiles(R.home())
#' listFiles(R.home(), pattern='README')
#' @export
listFiles <- function(x, ...) base::list.files(path=x, full.names=TRUE, ...)
