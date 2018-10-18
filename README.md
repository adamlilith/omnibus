# omnibus
Utility functions for the 99%.

## Statistics and computation ##
* `art`: Aligned rank transform for using rank data in ANOVAs
* `geoMean`: Geometric mean.
* `logitAdj` and `probitAdj`: Logit and probit transform robust to values that equal 0 or 1.
* `mmode`: Modal value
* `sampleAcross`: Permute values across two vectors or columns in two data frames or matrices.
* `sampleStrat`: Stratified randomization where strata are continuous values.
* `se`: Standard error

## Geometry ##
* `pairDist`: Pairwise Euclidian distance between two sets of points.
* `quadArea`: Area of a quadrilateral.

## Data manipulation ##
* `combineDf`: Combine data frames with different schema.
* `cull`: Force vectors or rows to have the same lenth or number of rows.
* `insertCol`: Insert a column in a matrix or data frame.
* `mergeList`: Merge lists
* `naOmitMulti`: Remove elements of multiple vectors if at least one element is `NA` or rows of matrices/data frames if at least one row has an `NA`.
* `naRows`: Indices of rows with at least one `NA`.
* `rotMatrix`: Rotate a matrix.
* `stretchMinMax`: Rescale values to a given range.

## Data properties and analysis ##
* `countDecDigits`: Count number of digits after a decimal.
* `histOverlap`: Histogram with overlapping or arbitrary bins.
* `longRun`: Longest run of a given sequence in a vector.
* `rankMulti`: Rank using secondary, tertiary, etc. fields as tie-breakers.
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

Adam
