#' Convert length or areal units
#'
#' @description This function converts length and area values from one unit to another (e.g., meters to miles, or square yards to acres). Alternatively, it provides the conversion factor for changing one unit to another.
#'
#' @param from,to Character: Names of the units to convert from/to. Partial matching is used, and case is ignored. Valid values are listed below. The \code{'*2'} values represent areas (e.g., \code{'m2'} is "meters-squared").
#' \itemize{
#' \item \code{'m'} or \code{'meters'}
#' \item  \code{'m2'} or \code{'meters2'}
#' \item \code{'km'} or \code{'kilometers'}
#' \item \code{'km2'} or \code{'kilometers2'}
#' \item \code{'mi'} or \code{'miles'}
#' \item \code{'mi2'} or \code{'miles2'}
#' \item \code{'ft'} or \code{'feet'}
#' \item \code{'ft2'} or \code{'feet2'}
#' \item \code{'yd'} or \code{'yards'}
#' \item \code{'yd2'} or \code{'yards2'}
#' \item \code{'ac'} or \code{'acres'}
#' \item \code{'ha'} or \code{'hectares'}
#' \item \code{'nmi'} or \code{'nautical miles'}
#' \item \code{'nmi2'} or \code{'nautical miles2'}
#' }
#'
#' @param x Numeric or \code{NULL} (default). The value(s) to convert in the unit specified by \code{from}. If left as \code{NULL}, the conversion factor is reported instead.
#'
#' @seealso \code{\link{expandUnits}}, \code{\link{conversionFactors}}
#' 
#' @examples
#' 
#' # conversion
#' convertUnits(from = 'm', to = 'km', 250)
#' convertUnits(from = 'm', to = 'mi', 250)
#' convertUnits(from = 'm2', to = 'km2', 250)
#'
#' # conversion factors 
#' convertUnits(from = 'm', to = 'km')
#' convertUnits(from = 'm')
#' convertUnits(to = 'm')
#'
#' @export
convertUnits <- function(from = NULL, to = NULL, x = NULL) {

	conversionFactors <- NULL
	utils::data('conversionFactors', envir = environment(), package = 'omnibus')

	units <- sort(unique(conversionFactors$from))

	if (!is.null(from)) {

		from <- expandUnits(from)
		from <- pmatchSafe(from, units, useFirst = TRUE, nmax = 1)

	}

	if (!is.null(to)) {
	
		to <- expandUnits(to)
		to <- pmatchSafe(to, units, useFirst = TRUE, nmax = 1)

	}
	
	if (is.null(x)) {
		
		if (!is.null(from) & !is.null(to)) {
			out <- conversionFactors$factor[conversionFactors$from == from & conversionFactors$to == to]
		} else if (!is.null(from) & is.null(to)) {
			out <- conversionFactors[conversionFactors$from == from, ]
		} else if (is.null(from) & !is.null(to)) {
			out <- conversionFactors[conversionFactors$to == to, ]
		}
			
	} else {
		f <- conversionFactors$factor[conversionFactors$from == from & conversionFactors$to == to]
		out <- f * x
	}
	out

}
