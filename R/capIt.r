#' Capitalize first letter of a string
#'
#' Capitalize the first letter of a string or the first letters of a list of strings.
#'
#' @param x Character or character vector.
#' @return Character or character vector.
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

