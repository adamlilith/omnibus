# omnibus

<!-- badges: start -->

[![R-CMD-check](https://github.com/adamlilith/omnibus/workflows/R-CMD-check/badge.svg)](https://github.com/adamlilith/omnibus/actions)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran version](https://www.r-pkg.org/badges/version/omnibus)](https://cran.r-project.org/package=omnibus)

<!-- badges: end -->


R Utility Functions for the 99%

<a href="https://adamlilith.github.io/omnibus/"><img src="man/figures/logo.png" align="right" height="223" alt=""/></a>

An assortment of helper functions for managing data (e.g., rotating values in matrices by a user-defined angle, switching from row- to column-indexing), dates (e.g., intuiting year from messy date strings), handling missing values (e.g., removing elements/rows across multiple vectors or matrices if any have an NA), text (e.g., flushing reports to the console in real-time); and combining data frames with different schema (copying, filling, or concatenating columns or applying functions before combining).

## Installation ##
As of version 1.1.2, `omnibus` is on CRAN! You can install the latest version on CRAN using:

`devtools::install.padckages('omnibus')`

... or the development version with:

`remotes::install_github('adamlilith/omnibus', dependencies = TRUE)`

You may need to install the `remotes` package first.

## Data manipulation ##
* `appendLists()`: Merge lists, appending elements with the same name.
* `bracket()`: Find values that bracket a given number.
* `combineDf()`: Combine data frames with different schema.
* `compareFloat()`: Compare two values to within floating-point precision.
* `convertUnits()`: Convert length or areal units.
* `conversionFactors()`: Data frame of conversion factors for length or areal units.
* `corner()`: Corner of a matrix or data frame.
* `cull()`: Force vectors or matrices/data frames to have the same length or number of rows.
* `expandUnits()`: Convert unit abbreviations to proper unit names.
* `insert()`: Insert values into a vector.
* `insertCol` and `insertRow()`: Insert column(s)/row(s) in a matrix or data frame.
* `maxRuns()`: Maximum number of continuous "runs" of values meeting a particular condition.
* `mergeLists()`: Merge lists, with precedence for elements of one list over another if they have the same names.
* `mirror()`: Flip an object left-right (or up-down).
* `mmode()`: Modal value(s).
* `notIn()` and `%notin%`: Opposite of `%in%`.
* `renameCol()`: Rename a column.
* `renumSeq()`: Renumber a sequence.
* `rotateMatrix()`: Rotate a matrix.
* `roundTo()`: Rounds a value to the nearest target value.
* `rowColIndexing()`: Convert between row and column indexing of a matrix.
* `side()`: Left/right side of a data frame or matrix.
* `stretchMinMax()`: Rescale values to a given range.
* `unlistRecursive()`: For any object in a list that is also a list, unlist it.
* `unragMatrix()`: Turn a "ragged" matrix into a "ragged" vector.


## Dates
* `domLeap()`: Data frame of days of each month in a leap year.
* `domNonLeap()`: Data frame of days of each month in a non-leap year.
* `doyLeap()`: Data frame of days of year in a leap year.
* `doyNonLeap()`: Data frame of days of year in a non-leap year.
* `isLeapYear()`: Is a year a leap year?
* `yearFromDate()`: Attempt to find the year across dates with non-standard formats.

## Geometry
* `pairDist()`: Pairwise Euclidean distance between two sets of points.
* `quadArea()`: Area of a quadrilateral.

## Handling `NA`s
* `%<na%`, `%<=na%`, `%==na%`, `%!=na%`, `%>na%`, and `%>=na%`: Comparative operations (>, >=, ==, !=, <, <=) but returns `FALSE` for `NA` cases (versus `NA`).
* `isTRUENA` and `isFALSENA()`: Logical operators robust to `NA`.
* `naCompare()`: Comparative operations (>, >=, ==, !=, <, <=) but returns `FALSE` for `NA` cases (versus `NA`).
* `naOmitMulti()`: Remove elements of multiple vectors if at least one element is `NA` or rows of matrices/data frames if at least one row has an `NA`.
* `naRows()`: Indices of rows with at least one `NA`.

## Data properties
* `countDecDigits()`: Count number of digits after a decimal.
* `is.wholeNumber()`: Is a numeric value a whole number, to within floating-point precision?
* `longRun()`: Longest run of a given sequence in a vector.
* `roundedSigDigits()`: Infers the number of significant digits represented by a decimal representation of a division operation or digits place to which an integer was rounded.
* `which.pmax` and `which.pmin()`: Combine `which.max` with `pmax` and `which.min` with `pmin` (vectorized `which.max` and `which.min`).

## System
* `dirCreate()`: Nicer version of `dir.create()`.
* `eps()`: Smallest floating point value your computer can think of.
* `listFiles()`: Nicer version of `list.files()`.
* `memUse()`: Display largest objects in memory.

## Text
* `capIt()`: Capitalize first letter of a string.
* `forwardSlash()`: Convert backslashes to forward slashes.
* `pmatchSafe()`: Partial matching of strings with error checking.
* `prefix()`: Add repeating character to a string to ensure it has a user-defined length (e.g., `7` --> `007`).
* `rstring()`: Make a nearly-guaranteed unique string.
* `say()`: Replacement for `print('abc'); flush.console()`.

Adam
