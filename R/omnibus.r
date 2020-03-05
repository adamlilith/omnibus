#' omnibus: Fantabulous helper functions
#'
#' This package contains a set of helper functions.
#'
#' Create an issue on \href{https://github.com/adamlilith/omnibus/issues}{GitHub}.
#' @details
#' @section Data manipulation:
#' 		\code{\link{combineDf}}: Combine data frames with different schema. \cr
#' 		\code{\link{corner}}: Corner of a matrix or data frame. \cr
#' 		\code{\link{cull}}: Force vectors or rows to have the same length or number of rows. \cr
#' 		\code{\link{insertCol}}: Insert column(s) in a matrix or data frame. \cr
#' 		\code{\link{cull}}: Force vectors or rows to have the same length or number of rows. \cr
#' 		\code{\link{insertCol}}: Insert a column in a matrix or data frame. \cr
#' 		\code{\link{mergeList}}: Merge lists. \cr
#' 		\code{\link{mirror}}: Mirror image of text or numeric value. \cr
#' 		\code{\link{rotateMatrix}}: Rotate a matrix. \cr
#' 		\code{\link{stretchMinMax}}: Rescale values to a given range. \cr
#' @section Dates:
#' 		\code{\link{doyLeap}}: Data frame of days of year in a leap year. \cr
#' 		\code{\link{doyLeapNonLeap}}: Data frame of days of year in a non-leap year. \cr
#'		\code{\link{isLeapYear}}: Is a year a leap year?  \cr
#' 		\code{\link{yearFromDate}}: Attempt to find the year across dates with non-standard formats. \cr
#' @section Geometry:
#' 		\code{\link{pairDist}}: Pairwise Euclidean distance between two sets of points. \cr
#'		\code{\link{quadArea}}: Area of a quadrilateral. \cr
#' @section Handling \code{NAs}:
#' 		\code{\link{\%<\%}}, \code{\link{\%<=na\%}}, \code{\link{\%==na\%}}, \code{\link{\%!=na\%}}, \code{\link{\%>na\%}}, and \code{\link{\%>=na\%}}: Comparative operations (\code{>}, \code{>=}, \code{==}, \code{!=}, \code{<}, \code{<=}) but returns \code{FALSE} for \code{NA} cases (versus \code{NA}). \cr
#' 		\code{\link{isTRUENA}} and \code{\link{isFALSENA}}: Logical operators robust to \code{NA}. \cr
#' 		\code{\link{naCompare}}: Comparative operations (\code{>}, \code{>=}, \code{==}, \code{!=}, \code{<}, \code{<=}) but returns \code{FALSE} for \code{NA} cases (versus \code{NA}). \cr
#' 		\code{\link{naOmitMulti}}: Remove elements of multiple vectors if at least one element is \code{NA} or rows of matrices/data frames if at least one row has an \code{NA}. \cr
#' 		\code{\link{naRows}}: Indices of rows with at least one \code{NA}. Same as \code{which(!complete.cases(x))}. \cr
#' @section Data properties:
#' 		\code{\link{countDecDigits}}: Count number of digits after a decimal. \cr
#' 		\code{\link{longRun}}: Longest run of a given sequence in a vector. \cr
#' 		\code{\link{roundedSigDigits}}: Infers the number of significant digits represented by a decimal representation of a division operation or digits place to which an integer was rounded. \cr
#' 		\code{\link{same}}: Are all elements of a vector the same? \cr
#' 		\code{\link{whichPMax}} and \code{\link{whichPMin}}: Combine \code{\link{which.max}} with \code{\link{pmax}}, and \code{\link{which.min}} with \code{\link{pmin}} (vectorized \code{\link{which.max}} and \code{\link{which.min}}). \cr
#' @section Function tools:
#' 		\code{\link{ellipseNames}}: Get names in \code{...} arguments. \cr
#' @section System:
#' 		\code{\link{dirCreate}}: Nicer version of \code{\link{dir.create}} \cr
#' 		\code{\link{eps}}: Smallest floating point value your computer can think of. \cr
#' 		\code{\link{listFiles}}: Nicer version of \code{\link{list.files}}. \cr
#' @section Text:
#' 		\code{\link{capIt}}: Capitalize first letter of a string. \cr
#' 		\code{\link{prefix}}: Add repeating character to a string to ensure it has a user-defined length (e.g., \code{7} --> \code{007}). \cr
#' 		\code{\link{say}}: Replacement for \code{print('abc'); flush.console}. \cr
#' @docType package
#' @author Adam B. Smith
#' @name omnibus
NULL
