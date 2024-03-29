% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/say.r
\name{say}
\alias{say}
\title{Nicer version of \code{print()} or \code{cat()} function}
\usage{
say(
  ...,
  pre = 0,
  post = 1,
  breaks = NULL,
  wiggle = 10,
  preBreak = 1,
  level = NULL,
  deco = "#"
)
}
\arguments{
\item{...}{Character strings to print}

\item{pre}{Integer >= 0.  Number of blank lines to print before strings}

\item{post}{Integer >= 0. Number of blank lines to print after strings}

\item{breaks}{Either \code{NULL}, which causes all strings to be printed on the same line (no wrap overflow) or a positive integer which wraps lines at this character length (e.g., \code{breaks=80} inserts line breaks every 80 characters).}

\item{wiggle}{Integer >- 0.  Allows line to overrun \code{breaks} length in characters before inserting line breaks.}

\item{preBreak}{If wrapping long lines indicates how subsequent lines are indented. NULL causes lines to be printed starting at column 1 on the display device. A positive integer inserts \code{preBreak} number of spaces before printing each line. A string causes each line to start with this string.}

\item{level}{Integer or \code{NULL}. If \code{NULL}, then the items in ... are displayed as-is. Otherwise, a value of 1, 2, or 3 indicates teh heading level, with lower numbers causing more decoration and spacing to be used.}

\item{deco}{Character. Character to decorate text with if \code{level} is not \code{NULL}.}
}
\value{
Nothing (side effect is output on the display device).
}
\description{
This function is a nicer version of \code{print()} or \code{cat()}, especially when used inline for functions because it displays immediately and pastes all strings together. It also does some rudimentary but optional word wrapping.
}
\examples{

say('The quick brown fox ', 'jumps over the lazy ', 'Susan.')
say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', breaks=10)
say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', level=1)
say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', level=2)
say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', level=3)

}
