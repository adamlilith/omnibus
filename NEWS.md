omnibus 0.3.4.1 (2020-XX-XX)

* Added function insertRow().

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
