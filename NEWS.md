omnibus 1.2.14 (2024-XX-XX)
===========
o Added function `unragMatrix()`.

omnibus 1.2.12 (2024-05-16)
===========
o Changed arguments of `mergeLists()` to `...` so it can handle >2 lists at a time
o Added `appendLists()` for combining lists with elements that may have the same names
o `roundedSigDigits()` works for numbers expressed in scientific notation

omnibus 1.2.11 (2024-04-03)
===========
o Added functions `compareFloat()` and aliases
o Added function `is.wholeNumber()`
o Added function `forwardSlash()`
o Fixed bug in `convertUnits()` for converting between meters and kilometers

omnibus 1.2.10 (2024-03-10)
===========
o Internal fix to `pmatchSafe()` to avoid conflicts with other packages.

omnibus 1.2.9 (2024-01-08)
===========
o Made `renumSeq()` faster.

omnibus 1.2.8 (2023-11-16)
===========
o Added `convertUnits()`
o Added `conversionFactors` data frame object
o Added `pmatchSafe()`
o Added `unlistRecursive()`

omnibus 1.2.7 (2023-08-21)
===========
o Updated `omnibus.r` file for compliance with CRAN

omnibus 1.2.6
===========
o Updated help for several functions

omnibus 1.2.5
===========
o Updated combineDf() for sophisticated combination of data frames
o combineDf() no longer compatible with prior versions of omnibus
o Added notIn() and %notin%

omnibus 1.1.3
===========
o Added insert()
o Added renumSeq()
o Fixed bug in insertCol()

omnibus 1.1.2
===========
o Removed clear()

omnibus 1.1.1
===========
o Removed same()
o Cleaned up help
o First sumbission to CRAN
