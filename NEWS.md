omnibus 1.1.1 (2022-02-09)
* Removed same()
* Cleaned up help
* Submission to CRAN!

omnibus 0.3.4.9 (2022-01-31)
* Added rowColIndexing()

omnibus 0.3.4.8 (2021-11-09)
* Added maxRuns()

omnibus 0.3.4.7 (2020-10-16)
* Can now specify inner/outer part of bracket in bracket()

omnibus 0.3.4.6 (2020-10-16)
* Fixed bug in bracket()

omnibus 0.3.4.5 (2020-10-11)
* Added roundTo()

omnibus 0.3.4.4 (2020-10-05)
* Fixed bug in stretchMinMax() when NAs are in input

omnibus 0.3.4.3 (2020-09-29)
* Added function bracket()
* Unexported function ellipseNames()

omnibus 0.3.4.2 (2020-08-18)
* Added function clear().
* Renamed which.pmax() and which.pmin()

omnibus 0.3.4.1 (2020-05-18)

* Added function insertRow().
* Recompiled for R 4.0.0.

omnibus 0.3.4.0 (2020-03-05)
* Added function isLeapYear().

omnibus 0.3.3.8 (2020-02-08)

* Fixed bug in whichPMax() and whichPMin when vectors were one unit long.

omnibus 0.3.3.6 (2020-02-06)
* Added function isTRUENA() and isFALSE() which vectorize isTRUE() and isFALSE().
* naCompare() and operators like %<na% are now compiled for speed.
* insertCol() now works if the target has no rows.

omnibus 0.3.3.4 (2020-02-03)
* Added functions whichPMax() and whichPMin() for vectorized which.max() and which.min().
* Added operators for naCompare(): %<na%, %<=na%, %==na%, %!=na%, %>na%, and %>=na%.

omnibus 0.3.3.3 (2020-01-31)
* Fixed bug in roundedFrom()... now named roundedSigDigits() (Thanks, Andria!).

omnibus 0.3.3.2 (2020-01-31)
* Added function mirror() which returns mirror image of something.
* Added function roundedFrom() which infers number of significant digits.

omnibus 0.3.3.1 (2020-01-29)
* Added function naCompare() for "NA-safe" comparative operations (>, >=, ==, !=, <, <=).
