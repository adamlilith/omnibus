#' Capitalize first letter of a string
#'
#' This function capitalizes the first letter of a string or the first letters of a list of strings.
#' @param x Character or character list.
#' @return Character or character list.
#' @seealso \code{\link{toupper}}, \code{\link{tolower}}, 
#' @examples
#' x <- c('shots', 'were', 'exchanged at the ', 'hospital.')
#' capIt(x)
#' @export

capIt <- function(x) {

    capWorker <- function(x) {
        s <- strsplit(x, ' ')[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep='', collapse=' ')
    }

    sapply(x, capWorker)
}

