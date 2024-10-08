% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unragMatrix.r
\name{unragMatrix}
\alias{unragMatrix}
\title{Turn a "ragged" matrix into a "ragged" vector}
\usage{
unragMatrix(x, skip = NA)
}
\arguments{
\item{x}{A matrix.}

\item{skip}{\code{NA} (default), \code{NULL}, or a numeric, integer, or character value. Value to not include in the output. If \code{NULL}, then \emph{no} values will be skipped.}
}
\value{
A list with one vector per matrix, plus 1) a vector named \code{startIndex} with indices of start values, and 2) a vector named \code{row} with one value per non-\code{skip} value in each matrix.
}
\description{
This function turns a "ragged" matrix into a vector. Consider a case where you have a matrix that looks like:
\cr\cr
1, 0,  1\cr
2, 3,  NA\cr
NA, 4, NA\cr
\cr
Here, each row represents a series of values, where missing values are represented by \code{NA}. This can be turned into a vector form going from left to right and top to bottom of the matrix, as in \code{c(1, 0, 1, 2, 3, 4)}, plus a vector \code{c(1, 4, 6)}, which provides the index of the first non-\code{NA} value in each row of the matrix in the vector, plus another vector, \code{c(1, 1, 1, 2, 2, 3)}, indicating the row to which each value in the vector belonged.
}
\examples{

# default
x <- matrix(c(1, 0, 1, 2, 3, NA, NA, 4, NA), byrow = TRUE, nrow = 3)
unragMatrix(x)

# skip nothing
unragMatrix(x, skip = NULL)

# skips rows with all "skip" values
y <- matrix(c(1, 0, 1, NA, NA, NA, NA, 4, NA), byrow = TRUE, nrow = 3)
unragMatrix(y)

}
