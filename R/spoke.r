#' Spoke plot
#'
#' This function Creates a "spoke" plot to visualize networks or correlation matrices. Factors are arranged in a circle, and lines connect them if they are linked in some manner (e.g., highly correlated). Two types of spokes formats can be used (e.g., one for positive correlations and negative correlations).
#' @param pos Binary matrix. Elements must be 1s indicating row (label) is associated with column (label) or 0s indicatig no such associations.
#' @param neg As \code{pos}, but indicating "negative" associations (however defined).
#' @param ontop Character. If \code{'pos'} then plot positive association spokes first; if \code{'neg'} then plot negative associations first.
#' @param labels Character vector. Names to add to plot. If NULL then column names of \code{pos} will be used (if any).
#' @param shrink Numeric. Relative size of non-label part of plot. This is useful if labels are too long to fit onto a plot. Default = 1.
#' @param labelOffset Numeric. Indicates how far from points labels are placed. If 1, then labels are placed on points, >1 then labels are placed outside points, and if <1 inside points.
#' @param nudge Factor by which to multiple y-coordinates of labels. Default is 1. Useful if there are many labels and they tend to overlap one another.
#' @param pch Integer. Point style (leave as \code{NA} to plot no points).
#' @param cexPoints Positive integer. Size of points
#' @param cexLabel Integer. Size of labels
#' @param lwdPos Numeric. Line width of "positive" spokes.
#' @param lwdNeg Numeric. Line width of "negative" spokes.
#' @param ltyPos Integer or character. Line style of "positive" spokes (see \code{?lines}).
#' @param ltyNeg Integer or character. Line style of "negative" spokes (see \code{?lines}).
#' @param colPos Integer or character. Color of "positive" spokes.
#' @param colNeg Integer or character. Color of "negative" spokes.
#' @param colPoints Integer or character. Color of points.
#' @param colLabel Integer or character. Color of labels.
#' @param ... Further arguments to pass to \code{plot()}, \code{points()}, \code{lines()}, or \code{text()}.
#' @return None. By-product is a spoke plot.
#' @examples
#' # create matrix of correlation coefficients from some data
#' d <- data.frame(a=1:100, b=(1:100)^2, c=-(1:100), d=(1:100)^3, e=-(1:100)^3)
#' correl <- cor(d)
#' pos <- correl >= 0.95
#' neg <- correl <= -0.95
#' spoke(pos, neg)
#' legend('bottomright', legend=c('Positive', 'Negative'), lty=c('solid', 'dashed'))
#' @export

spoke <- function(
	pos,
	neg = NULL,
	ontop = 'pos',
	labels = NULL,
	shrink = 1,
	labelOffset = 1.02,
	nudge = 1,
	pch = 16,
	cexPoints = 1,
	cexLabel = 1,
	lwdPos = 1,
	lwdNeg = 1,
	ltyPos = 'solid',
	ltyNeg = 'dashed',
	colPos = 'black',
	colNeg = 'black',
	colPoints = 'black',
	colLabel = 'black',
	...
) {

	#############
	### setup ###
	#############

	offset <- 0.2 # amount by which to slide entire plot to left and lower to ensure it's more toward the center of the plotting region

	####################
	## PRE-PROCESSING ##
	####################

	plot(x=0, y=0, col=par('bg'), type='n', axes=FALSE, xlab=NA, ylab=NA, xlim=c(-1.1 * shrink * labelOffset, shrink * labelOffset), ylim=c(-1.1 * shrink * labelOffset, shrink * labelOffset), ...)

	##########
	## MAIN ##
	##########

	## get coordinates for connection points
	xLink <- rev(shrink * cos(seq(pi / 2, 2 * pi + pi / 2, length.out=ncol(pos) + 1)))
	yLink <- rev(shrink * sin(seq(pi / 2, 2 * pi + pi / 2, length.out=ncol(pos) + 1)))

	## add points
	if (!is.na(pch)) points(xLink - offset * shrink, yLink, pch=pch, cex=cexPoints, col=colPoints, xpd=NA, ...)

	if (ontop=='pos') {

		## add negative spokes
		if (!is.null(neg)) {
			for (i in 1:nrow(neg)) {
				for (j in 1:ncol(neg)) {
					if (!is.na(neg[i, j]) && neg[i, j]==1) lines(x=c(xLink[i] - offset * shrink, xLink[j] - offset * shrink), y=c(yLink[i], yLink[j]), col=colNeg, lwd=lwdNeg, lty=ltyNeg, xpd=NA, ...)
				}
			}
		}

		## add positive spokes
		for (i in 1:nrow(pos)) {
			for (j in 1:ncol(pos)) {
				if (!is.na(pos[i, j]) && pos[i, j]==1) lines(x=c(xLink[i] - offset * shrink, xLink[j] - offset * shrink), y=c(yLink[i], yLink[j]), col=colPos, lwd=lwdPos, lty=ltyPos, xpd=NA, ...)
			}
		}
		
	} else {

		## add positive spokes
		for (i in 1:nrow(pos)) {
			for (j in 1:ncol(pos)) {
				if (!is.na(pos[i, j]) && pos[i, j]==1) lines(x=c(xLink[i] - offset * shrink, xLink[j] - offset * shrink), y=c(yLink[i], yLink[j]), col=colPos, lwd=lwdPos, lty=ltyPos, xpd=NA, ...)
			}
		}

		## add negative spokes
		if (!is.null(neg)) {
			for (i in 1:nrow(neg)) {
				for (j in 1:ncol(neg)) {
					if (!is.na(neg[i, j]) && neg[i, j]==1) lines(x=c(xLink[i] - offset * shrink, xLink[j] - offset * shrink), y=c(yLink[i], yLink[j]), col=colNeg, lwd=lwdNeg, lty=ltyNeg, xpd=NA, ...)
				}
			}
		}

	}
		
	## add labels
	if (is.null(labels)) if (class(pos)=='matrix') { labels <- colnames(pos) } else { labels <- names(pos) }

	xLabel <- rev(shrink * labelOffset * cos(seq(pi / 2, 2 * pi + pi / 2, length.out=ncol(pos) + 1)))
	yLabel <- rev(shrink * labelOffset * sin(seq(pi / 2, 2 * pi + pi / 2, length.out=ncol(pos) + 1)))
	yLabel <- yLabel * nudge

	position <- rep(1, ncol(pos))
	position[xLabel > 0] <- 4
	position[xLabel < 0] <- 2
	position[xLabel < 10^-5 & xLabel > -10^-5 & yLabel > 0] <- 3
	position[xLabel < 10^-5 & xLabel > -10^-5 & yLabel < 0] <- 1

	text(x=xLabel - offset * shrink, y=yLabel, labels=labels, pos=position, cex=cexLabel, col=colLabel, xpd=NA, ...)

}
