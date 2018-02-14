#' Plots a pie graph.
#'
#' This function plots a pie graph. Unlike \code{\link[graphics]{pie}}, it can be used to add a pie graph to an existing plot.
#' x a Numeric vector of non-negative numerical quantities. These values are displayed as the areas of pie slices.
#' add Logical, if \code{TRUE} then add the pie to an existing plot. If \code{FALSE} then the arguments are passed to \code{\link[graphics]{pi}}. In this case then all arguments default to their interpretation in that function.
#' xPos Numeric, the x-coordinate of the center of the pie (ignored if \code{add} is \code{FALSE}).
#' yPos Numeric, the y-coordinate of the center of the pie (ignored if \code{add} is \code{FALSE}).
#' radius Numeric, radius of pie in plot units.
#' col Character or integer, color(s) with which to fill pie slices.
#' border Character or integer, color(s) with which to draw border of pie slices.
#' aspect Logical, if \code{TRUE} (default) then force a 1:1 aspect ratio of the pie relative to the plot axes. If the aspect ratio of the plot is not 1:1 then \code{radius} will equal the radius along the x-axis and the y-axis radius will be shrunk.
#' ... Arguments to pass to \code{link[graphics]{polygon}} if |code{add = TRUE} or \code{link[graphics]{pie}} if \code{add = FALSE}.

#' @seealso \code{\link{graphics::pie}} \code{\link{graphics::polygon}}
#' @examples
#' plot(0, 0, xlim=c(-1, 1), ylim=c(-1, 1))
#' pies(1:3, add=TRUE)
#' pies((1:10)^2, add=TRUE, xPos=1, yPos=0.5, radius=0.2)
#' pies(c(1, 10), add=TRUE, xPos=-0.5, yPos=0, radius=0.4)
#' @export
pies <- function(
	x,
	add = FALSE,
	xPos = 0,
	yPos = 0,
	radius = 0.8,
	col = base::seq_along(x),
	border = 'black',
	aspect = TRUE,
	...
) {

	if (!add) {
		pie(...)
	} else {

		total <- x / sum(x)
		deg <- 360 * cumsum(total)
		deg <- c(0, deg)

		plotBox <- par('usr')
		ratio <- (plotBox[4] - plotBox[3]) / (plotBox[2] - plotBox[1])
		
		for (i in 1:length(x)) {
		
			theta <- seq((-deg[i] + 90) * pi / 180, (-deg[i + 1] + 90) * pi / 180, length.out=999)
			xs <- xPos + radius * cos(theta)
			ys <- yPos + ratio * radius * sin(theta)
			
			xs <- c(xPos, xs, xPos)
			ys <- c(yPos, ys, yPos)
			
			graphics::polygon(xs, ys, col=col[i], border=border, ...)
		
		}
		
	}

}


