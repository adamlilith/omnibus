#' Plot an annulus on an existing plot.
#'
#' This function adds an annulus to an existingg plot.
#' @param x Numeric, x-coordinate of center of annulus.
#' @param y Numeric, y-coordinate of center of annulus.
#' @param inner Numeric, inner radius.
#' @param outer Numeric, outer radius.
#' @param deg Two-element numeric list, degrees across which to draw the annulus (0 is "north" and increasing values go clockwise). Default is \code{c(0, 360)}.
#' @param n Integer, number of vertices used to approximate a circle.
#' @param col Character or integer, color used to fill annulus.  Note that if \code{col} and \code{border} are different colors and \code{deg} is \code{c(0, 360)} (the default), then you wil abel to see the line connecting the inner and outer borders.
#' @param border Character or integer, color used to draw annulus border.  Note that if \code{col} and \code{border} are different colors and \code{deg} is \code{c(0, 360)} (the default), then you wil abel to see the line connecting the inner and outer borders.
#' @param force0 Logical, if \code{TRUE} then negative values of inner and outer are coerced to equal 0. If \code{FALSE} then throws an error.
#' @param ... Arguments to send to [graphics::polygon()].
#' @return Nothing (side-effect is to add an annulus to an existing plot).
#' @seealso \code{\link{graphics::polygon()}}
#' @examples
#' plot(0, 0, xlim=c(-1, 1), ylim=c(-1, 1))
#' annulus(x=0.3, y=-0.2, inner=1, outer=0.6, col='cornflowerblue')
#' annulus(x=-0.3, y=0.2, inner=1, outer=0.6, deg=c(0, 135), col='firebrick1', border='black')
#' @export
annulus <- function(
	x,
	y,
	inner,
	outer,
	deg=c(0, 360),
	n=1000,
	col='black',
	border=col,
	force0=FALSE,
	...
) {

	### catch errors
	if (force0 & inner < 0) {
		inner <- 0
		warning('Forcing inner radius to 0 because <0.')
	}
	
	if (force0 & outer < 0) {
		outer <- 0
		warning('Forcing outer radius to 0 because <0.')
	}
	
	### calculate innner/outer vertices in polar coordinates
	theta <- seq((-deg[1] + 90) * pi / 180, (-deg[2] + 90) * pi / 180, length.out=n)
	outerVerticesX <- x + outer * cos(theta)
	outerVerticesY <- y + outer * sin(theta)
	innerVerticesX <- x + inner * cos(theta)
	innerVerticesY <- y + inner * sin(theta)
	
	### concatenate coordinates for outer/inner circles
	xCoords <- c(outerVerticesX, rev(innerVerticesX))
	yCoords <- c(outerVerticesY, rev(innerVerticesY))

	### plot
	polygon(xCoords, yCoords, col=col, border=border)

}

