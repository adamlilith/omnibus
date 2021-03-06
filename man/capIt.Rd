% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capIt.r
\name{capIt}
\alias{capIt}
\title{Capitalize first letter of a string}
\usage{
capIt(x)
}
\arguments{
\item{x}{Character or character list.}
}
\value{
Character or character list.
}
\description{
This function capitalizes the first letter of a string or the first letters of a list of strings.
}
\examples{
x <- c('shots', 'were', 'exchanged at the ', 'hospital.')
capIt(x)
}
\seealso{
\code{\link{toupper}}, \code{\link{tolower}},
}
