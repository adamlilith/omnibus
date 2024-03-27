#' Replace backslash with forward slash
#'
#' This function is helpful for Windows systems, where paths are usually expressed with left slashes, whereas \code{R} requires right slashes.
#'
#' @param x A string.
#'
#' @return Character.
#' 
#' @examples
#' 
#' forwardSlash("C:\\ecology\\main project")
#'
#' @export
forwardSlash <- function(x) gsub("\\\\", "/", x)
