# omnibus
Utility functions for the 99%.

To install `omnibus` do the following:

`install.packages('devtools') # if you haven't done this already`  
`library(devtools)`  
`install_github('adamlilith/omnibus')`

NB: If for some reason these commands don't work, you can install the package(s) by downloading the latest zip/tar file from the `zipTarFiles` directory and installing the package(s) manually.

## Geometry ##
* `pairDist`: Pairwise Euclidean distance between two sets of points.
* `quadArea`: Area of a quadrilateral.

## Data manipulation ##
* `combineDf`: Combine data frames with different schema.
* `corner`: Corner of a matrix or data frame.
* `cull`: Force vectors or rows to have the same length or number of rows.
* `insertCol`: Insert column(s) in a matrix or data frame.
* `cull`: Force vectors or rows to have the same lenth or number of rows.
* `insertCol`: Insert a column in a matrix or data frame.
* `mergeList`: Merge lists
* `naOmitMulti`: Remove elements of multiple vectors if at least one element is `NA` or rows of matrices/data frames if at least one row has an `NA`.
* `naCompare`: Comparative operations (>, >=, ==, !=, <, <=) but returns `FALSE` for `NA` cases (versus `NA`).
* `naRows`: Indices of rows with at least one `NA`.
* `rotateMatrix`: Rotate a matrix.
* `stretchMinMax`: Rescale values to a given range.

## Data properties and analysis ##
* `countDecDigits`: Count number of digits after a decimal.
* `longRun`: Longest run of a given sequence in a vector.
* `same`: Are all elements of a vector the same?
* `yearFromDate`: Attempt to find the year across dates with non-standard formats.

## Function tools ##
* `ellipseNames`: Get names in `...` arguments.

## System ##
* `dirCreate`: Nicer version of `dir.create()`'
* `eps`: Smallest floating point value your computer can think of.
* `listFiles`: Nicer version of `list.files()`.

## Text ##
* `capIt`: Capitalize first letter of a string.
* `prefix`: Add repeating character to a string to ensure it has a user-defined length (e.g., `7` --> `007`).
* `say`: Replacement for `print('abc'); flush.console()`.

## Omnibus ##
* `mirror`: Mirror image of text or numeric value.

Adam
