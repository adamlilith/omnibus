#' Replacement for dir.create()
#'
#' This function is a somewhat friendlier version of \code{\link{dir.create}} in that it automatically sets \code{recursive=TRUE} and \code{showWarnings=FALSE} arguments.
#' @param ... Character string(s). The path and name of the directory to create. Multiple strings will be pasted together into one path, although slashes will not be pasted between them.
#' @return Nothing (creates a directory on the storage system).
#' @seealso \code{\link{dir.create}}
#' @export

dirCreate <- function(...) base::dir.create(path=paste0(...), recursive=TRUE, showWarnings=FALSE)

