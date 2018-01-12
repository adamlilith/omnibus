#' Adds a gradient legend to a plot
#'
#' This function adds a legend to an existing plot that shows a gradient in color. It first draws a "containing" box then a bar with a color gradient inside the box. A legend title and labels for levels indicated by the color bar can be added.
#' @param x Numeric or character. Describes the location of the legend. This is a numeric value (in which case \code{y} must also be supplied) indicating the x-coordinate of the box surrounding the legend. Alternatively, it is a character describing the position of the box surrounding the legend relative to the existing plot (\code{'topleft'}, \code{'topright'}, \code{'bottomleft'},  or \code{'bottomright'}).
#' @param y Numeric or \code{NULL}.
#' @param inset Numeric. If \code{x} is a word descrbing the position of the legend, then this is the degree to which the legend is inset (or outset, if negative) relative to the figure's border. If two values are supplied then the first pertains to the horizontal offset and the second the vertial offset.
#' @param width Numeric. Scaling factor for box width (default is 1.2).
#' @param height Numeric. Scaling factor for box height (default is 1.2).
#' @param labels Vector of characters of numeric values. Labels (from least to most) of levels of the fcal variable indicated by the color ramp.
#' @param labAdj Numeric between 0 and 1. Position of labels relative to the containing box.
#' @param col Character list. Names of colors to be used to create a gradient to fill the legend bar. The first color will be the lowest value and the last the highest value.
#' @param border Character or integer. Name (or integer code) of color to use to draw border of the gradient bar.
#' @param title Character or \code{NULL}. Name of title for the legend.
#' @param titleAdj Two numeric values between 0 and 1. Position of the legend relative to the container box. The first pertains to horizontal positioning and the second vertial positioning.
#' @param gradAdjX Two numeric values between 0 and 1. Size of the gradient bar in the x-dimension as a proportion of the container box size. The first pertains to the left side of the bar and the second the right side.
#' @param gradAdjY Two numeric values between 0 and 1. Size of the gradient bar in the y-dimension as a proportion of the container box size. The first pertains to the bottom of the bar and the second the top.
#' @param boxBg Character or integer. Name (or integer code) of color to use to use for box containing legend. Leave as \code{NULL} to not draw a box.
#' @param boxBorder Character or integer. Name (or integer code) of color to use to use for box border containing legend. Leave as \code{NULL} to not draw a box border.
#' @param ... Arguments to pass to \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}}, or \code{\link[graphics]{text}}.
#' @return Nothing (side effect is to add a legend to an existing graphics device).
#' @seealso \code{\link[graphics]{legend}}
#' @examples
#' set.seed(123)
#' wealth <- data.frame(
#' 	country = c('USA', 'Japan', 'Malaysia', 'Germany', 'England',
#' 'South Korea', 'DR Congo', 'Nigeria'),
#' 	pop = c(325365189, 126672000, 31718000, 82175700,
#'  54786300, 51446201, 78736153, 185989640),
#' 	gdp = c(18558, 5420, 913.593, 4150, 1870, 2029, 70.294, 1166),
#' 	perCapGdp = c(145894, 57220, 28490, 50206, 34205, 39446, 811, 6351),
#' 	hivPerc = c(0.03, 0.01, 0.40, 0.15, 0.16, 0.29, 0.70, 2.90)
#' )
#'
#' # color ramp
#' colFx <- grDevices::colorRampPalette(c('white', 'red'))
#' cols <- colFx(100)
#' hivPerc <- round(100 * wealth$hivPerc / max(wealth$hivPerc))
#' cols <- cols[hivPerc]
#'
#' # rescale population (symbols size)
#' popRescaled <- 0.1 + 3 * (log10(wealth$pop) - min(log10(wealth$pop)))
#' plot(wealth$gdp, wealth$perCapGdp, pch=21, cex=popRescaled, bg=cols, xlab='GDP (Billion $)', ylab='GDP Per Capita ($)')
#' text(wealth$gdp, wealth$perCapGdp, labels=as.character(wealth$country), pos=4, xpd=NA)
#'
#' gradLegend(
#' 	x='bottomright',
#' 	y = NULL,
#' 	inset = 0.02,
#' 	width = 0.2,
#' 	height = 0.3,
#' 	labels = range(wealth$hivPerc),
#' 	labAdj = 0.7,
#' 	col = c('white', 'red'),
#' 	border = 'black',
#' 	title = 'HIV (%)',
#' 	titleAdj = c(0.5, 0.9),
#' 	gradAdjX = c(0.2, 0.5),
#' 	gradAdjY = c(0.1, 0.8),
#' 	boxBg = 'gray90',
#' 	boxBorder = 'black'
#' )
#' @export

gradLegend <- function(
	x,
	y = NULL,
	inset = 0,
	width = 0.07,
	height = 0.1,
	labels = NULL,
	labAdj = 0.75,
	col = c('white', 'black'),
	border = 'black',
	title = '',
	titleAdj = c(0.5, 0.9),
	gradAdjX = c(0.2, 0.5),
	gradAdjY = c(0.1, 0.8),
	boxBg = par('bg'),
	boxBorder = 'black',
	...
) {

	# get coordinate stats for existing plot
	pos <- par('usr')

	xRange <- pos[2] - pos[1]
	yRange <- pos[4] - pos[3]

	boxWidth <- width * xRange
	boxHeight <- height * yRange

	# get containing box location top left coordinate
	if (class(x) == 'character') {

		if (length(inset) == 1) inset <- c(inset, inset)

		xInset <- inset[1] * xRange
		yInset <- inset[2] * yRange

		if (x == 'topleft') {
			x <- pos[1] + xInset
			y <- pos[4] - yInset
		} else if (x == 'topright') {
			x <- pos[2] - boxWidth - xInset
			y <- pos[4] - yInset
		} else if (x == 'bottomleft') {
			x <- pos[1] + xInset
			y <- pos[3] + boxHeight + yInset
		} else if (x == 'bottomright') {
			x <- pos[2] - boxWidth - xInset
			y <- pos[3] + boxHeight + yInset
		}

	}

	# draw containing box
	graphics::polygon(c(x, x + width * xRange, x + width * xRange, x), c(y, y, y - height * yRange, y - height * yRange), col=boxBg, border=boxBorder, ...)

	# legend title
	graphics::text(x + titleAdj[1] * boxWidth, y - (1 - titleAdj[2]) * boxHeight, labels=title, ...)

	# color gradient
	colFx <- grDevices::colorRampPalette(col)
	cols <- colFx(99)

	# get gradient bounding box
	left <- x + gradAdjX[1] * boxWidth
	right <- x + gradAdjX[2] * boxWidth
	top <- y - (1 - gradAdjY[2]) * boxHeight
	bottom <- y - (1 - gradAdjY[1]) * boxHeight

	gradHeight <- top - bottom

	# plot (use many small rectangles)
	yInc <- seq(bottom, top, length.out=100)

	for (i in 1:99) graphics::polygon(c(left, right, right, left), c(yInc[i], yInc[i], yInc[i + 1], yInc[i + 1]), col=cols[i], border=NA, ...)
	if (!is.na(border)) graphics::polygon(c(left, right, right, left), c(bottom, bottom, top, top), col=NA, border=border, ...)

	# add labels
	if (!is.null(labels)) {

		labY <- seq(bottom, top, length.out=length(labels))
		text(x + boxWidth * rep(labAdj, length(labels)), labY, labels=labels, pos=4, ...)

	}

}
