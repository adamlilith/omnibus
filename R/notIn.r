#' Opposite of '\%in\%'
#'
#' @description Indicate if elements of a vector are not in another vector.
#'
#' @param x,table Vectors.
#' @return A logical vector.
#'
#' @examples
#'
#' x <- c('a', 'v', 'o', 'C', 'a', 'd', 'O')
#' y <- letters
#'
#' y %notin% x
#' x %notin% y
#'
#' @export
notIn <- compiler::cmpfun( function(x, table) !(x %in% table) )


#' @name `%notin%`
#' @title Opposite of '%in%'
#' @rdname notIn
#' @export
`%notin%` <- notIn
