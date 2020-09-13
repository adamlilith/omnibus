# omnibus
Utility functions for the 99%.

To install `omnibus` do the following:

`remotes::install_github('adamlilith/omnibus', dependencies=TRUE)`

NB: If for some reason these commands do not work, you can install the package(s) by downloading the latest zip/tar file from the `zipTarFiles` directory and installing the package(s) manually.

## Data manipulation ##
* `combineDf`: Combine data frames with different schema.
* `corner`: Corner of a matrix or data frame.
* `cull`: Force vectors or rows to have the same length or number of rows.
* `insertCol` and `insertRow`: Insert column(s)/row(s) in a matrix or data frame.
* `cull`: Force vectors or rows to have the same length or number of rows.
* `insertCol`: Insert a column in a matrix or data frame.
* `mergeLists`: Merge lists.
* `mirror`: Mirror image of text or numeric value.
* `rotateMatrix`: Rotate a matrix.
* `stretchMinMax`: Rescale values to a given range.

## Dates ##
* `domLeap`: Data frame of days of each month in a leap year. \cr
* `domNonLeap`: Data frame of days of each month in a non-leap year. \cr
* `doyLeap`: Data frame of days of year in a leap year.
* `doyNonLeap`: Data frame of days of year in a non-leap year.
* `isLeapYear`: Is a year a leap year?
* `yearFromDate`: Attempt to find the year across dates with non-standard formats.

## Geometry ##
* `pairDist`: Pairwise Euclidean distance between two sets of points.
* `quadArea`: Area of a quadrilateral.

## Handling NAs
* `%<na%`, `%<=na%`, `%==na%`, `%!=na%`, `%>na%`, and `%>=na%`: Comparative operations (>, >=, ==, !=, <, <=) but returns `FALSE` for `NA` cases (versus `NA`).
* `isTRUENA` and `isFALSENA`: Logical operators robust to `NA`.
* `naCompare`: Comparative operations (>, >=, ==, !=, <, <=) but returns `FALSE` for `NA` cases (versus `NA`).
* `naOmitMulti`: Remove elements of multiple vectors if at least one element is `NA` or rows of matrices/data frames if at least one row has an `NA`.
* `naRows`: Indices of rows with at least one `NA`.

## Data properties ##
* `countDecDigits`: Count number of digits after a decimal.
* `longRun`: Longest run of a given sequence in a vector.
* `roundedSigDigits`: Infers the number of significant digits represented by a decimal representation of a division operation or digits place to which an integer was rounded.
* `same`: Are all elements of a vector the same?
* `which.pmax` and `which.pmin`: Combine `which.max` with `pmax` and `which.min` with `pmin` (vectorized `which.max` and `which.min`).

## Function tools ##
* `ellipseNames`: Get names in `...` arguments.

## System ##
* `clear`: Erase everything in memory.
* `dirCreate`: Nicer version of `dir.create()`.
* `eps`: Smallest floating point value your computer can think of.
* `listFiles`: Nicer version of `list.files()`.
* `lsos`: Display largest objects in memory.

## Text ##
* `capIt`: Capitalize first letter of a string.
* `prefix`: Add repeating character to a string to ensure it has a user-defined length (e.g., `7` --> `007`).
* `say`: Replacement for `print('abc'); flush.console()`.

Adam
