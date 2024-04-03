#' Convert unit abbreviations to proper unit names
#'
#' @description This function converts abbreviations of length and area units (e.g., "m", "km", and "ha") to their proper names (e.g., "meters", "kilometers", "hectares"). Square areal units are specified using an appended "2", where appropriate (e.g., "m2" means "meters-squared" and will be converted to "meters2").
#'
#' @param x Character: Abbreviations to convert. Case is ignored.
#' \itemize{
#' \item \code{'m'} will be converted to \code{'meters'}
#' \item \code{'m2'} will be converted to \code{'meters2'}
#' \item \code{'km'} will be converted to \code{'kilometers'}
#' \item \code{'km2'} will be converted to \code{'kilometers2'}
#' \item \code{'mi'} will be converted to \code{'miles'}
#' \item \code{'mi2'} will be converted to \code{'miles2'}
#' \item \code{'ft'} will be converted to \code{'feet'}
#' \item \code{'ft2'} will be converted to \code{'feet2'}
#' \item \code{'yd'} will be converted to \code{'yards'}
#' \item \code{'yd2'} will be converted to \code{'yards2'}
#' \item \code{'ac'} will be converted to \code{'acres'}
#' \item \code{'ha'} will be converted to \code{'hectares'}
#' \item \code{'nmi'} will be converted to \code{'nautical miles'}
#' \item \code{'nmi2'} will be converted to \code{'nautical miles2'}
#' }
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
