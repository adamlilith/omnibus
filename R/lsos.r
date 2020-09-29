#' Size of objects taking most memory
#'
#' Displays the largest objects in memory.
#' @param n Positive integer, number of objects to display.
#' @param pattern Character, return size of objects with this pattern in their name.
#' @param orderBy Either \code{NULL} (default) or any of c\code{'Type'}, \code{'Size'}, \code{'PrettySize'}, \code{'Rows'}, or \code{'Columns'}. Controls field by which objects are sorted.
#' @param decreasing Logical, if \code{FALSE} (default), objects are displayed from larges to smallest.
#' @param pos Integer, indicates environment in which to search for objects. See help for \code{\link{ls}}.
#' @details Adapted from StackOverflow user Dirk Eddelbuettel (original function at https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session).
#' @return Nothing (displays a table).
#' @examples
#' lsos()
#' lsos(3)
#' @export

# shorthand
lsos <- function(n=10, ...) {
    .ls.objects(n=n, ..., orderBy='Size', decreasing=TRUE)
}

.ls.objects <- function (
	n,
	pattern,
	orderBy,
	decreasing=FALSE,
	pos = 1
) {

    napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos=pos)))
    names <- ls(pos=pos, pattern=pattern)
    
	obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.prettysize <- sapply(obj.size, function(r) prettyNum(r, big.mark=',') )
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    
	vec <- is.na(obj.dim)[, 1] & (obj.type != 'function')
    obj.dim[vec, 1] <- napply(names, length)[vec]
    
	out <- data.frame(obj.type, obj.size,obj.prettysize, obj.dim)
    names(out) <- c('Type', 'Size', 'PrettySize', 'Rows', 'Columns')
    
	if (!is.null(orderBy)) out <- out[order(out[[orderBy]], decreasing=decreasing), ]
    out <- out[c('Type', 'PrettySize', 'Rows', 'Columns')]
    names(out) <- c('Type', 'Size', 'Rows', 'Columns')
    out <- out[1:n, ]
	
	out

}

