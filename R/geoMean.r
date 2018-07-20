#' Geometric mean
#'
#' Calculate the geometric mean, with optional removal of \code{NA}'s and propagation of zeros (adapted from Paul McMurdie on StackOverflow)[https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in].)
#' @param x Numeric list.
#' @param na.rm Logical, if \code{TRUE} then remove \code{NA} values first.
#' @param prop0 Logical, if \code{FALSE} (default) then if any value in \code{x} equals 0 then the output will be zero. If \code{TRUE}, then zero values will be removed before calculation of the geometric mean.
#' @details The CBI is the Spearman rank correlation coefficient between the proportion of sites in each prediction class and the expected proportion of predictions in each prediction class based on the proportion of the landscape that is in that class. Values >0 indicate the model's output is positively correlated with the true probability of presence.  Values <0 indicate it is negatively correlated with the true probabilty of presence.
#' @references Boyce, M.S., Vernier, P.R., Nielsen, S.E., and Schmiegelow, F.K.A.  2002.  Evaluating resource selection functions.  \emph{Ecological Modeling} 157:281-300)
#' @references Hirzel, A.H., Le Lay, G., Helfer, V., Randon, C., and Guisan, A.  2006.  Evaluating the ability of habitat suitability models to predict species presences.  \emph{Ecological Modeling} 199:142-152.
#' @examples
#' @export
geoMean = function(x, na.rm=TRUE, prop0 = FALSE) {
	
	if(any(x < 0, na.rm = TRUE)){
		NaN
	} else if (prop0) {
		if (any(x == 0, na.rm = TRUE)) {
		  0
		} else {
			exp(mean(log(x), na.rm = na.rm))
		}
	} else {
		exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
	}
	
}
