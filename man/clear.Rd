% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clear.r
\name{clear}
\alias{clear}
\title{Erase everything in memory and perform garbage collection.}
\usage{
clear(rm = FALSE)
}
\arguments{
\item{rm}{Logical, if \code{FALSE} (default), everything will be cleared from memory. If \code{TRUE}, then nothing will be cleared but garbage collection will be implemented.}
}
\value{
Nothing (clears everything from memory).
}
\description{
This is a one-line function for the code: \code{rm(list=ls()); gc()}.
}
\examples{
clear(FALSE) # will not clear memory
}
