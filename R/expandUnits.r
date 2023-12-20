#' Convert unit abbreviations to proper unit names
#'
#' @description This function converts abbreviations of length and area units (e.g., "m", "km", and "ha") to their proper names (e.g., "meters", "kilometers", "hectares"). Square areal units are specified using an appended "2", where appropriate (e.g., "m2" means "meters-squared" and will be converted to "meters2").
#'
#' @param x Character: Abbreviations to convert. Case is ignored.
#' * \code{'m'} will be converted to \code{'meters'}
#' * \code{'m2'} will be converted to \code{'meters2'}
#' * \code{'km'} will be converted to \code{'kilometers'}
#' * \code{'km2'} will be converted to \code{'kilometers2'}
#' * \code{'mi'} will be converted to \code{'miles'}
#' * \code{'mi2'} will be converted to \code{'miles2'}
#' * \code{'ft'} will be converted to \code{'feet'}
#' * \code{'ft2'} will be converted to \code{'feet2'}
#' * \code{'yd'} will be converted to \code{'yards'}
#' * \code{'yd2'} will be converted to \code{'yards2'}
#' * \code{'ac'} will be converted to \code{'acres'}
#' * \code{'ha'} will be converted to \code{'hectares'}
#' * \code{'nmi'} will be converted to \code{'nautical miles'}
#' * \code{'nmi2'} will be converted to \code{'nautical miles2'}
#'
#' @seealso \code{\link{convertUnits}}, \code{\link{conversionFactors}}
#' 
#' @examples
#' 
#' expandUnits(c('m', 'm2', 'ac', 'nm2'))
#'
#' @export
expandUnits <- function(x) {

	x <- tolower(x)

	x[x == 'm'] <- 'meters'
	x[x == 'm2'] <- 'meters2'
	x[x == 'km'] <- 'kilometers'
	x[x == 'km2'] <- 'kilometers2'
	x[x == 'mi'] <- 'miles'
	x[x == 'mi2'] <- 'miles2'
	x[x == 'ft'] <- 'feet'
	x[x == 'ft2'] <- 'feet2'
	x[x == 'yd'] <- 'yards'
	x[x == 'yd2'] <- 'yards2'
	x[x == 'ac'] <- 'acres'
	x[x == 'ha'] <- 'hectares'
	x[x == 'nmi'] <- 'nautical miles'
	x[x == 'nmi2'] <- 'nautical miles2'
	x

}
