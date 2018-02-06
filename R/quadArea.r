#' Area of a quadrilateral
#'
#' Calculates the area of a quadrilateral by dividing it into two triangles and applying Heron's formula.
#' @param x Numeric list. \code{x} coordinates of quadrilateral.
#' @param y Numeric list. \code{y} coordinates of quadrilateral.
#' @return Numeric (area of a quadrilateral in same units as \code{x} and \code{y}.
#' @examples
#' x <- c(0, 6, 4, 1)
#' y <- c(0, 1, 7, 4)
#' quadArea(x, y)
#' plot(1, type='n', xlim=c(0, 7), ylim=c(0, 7), xlab='x', ylab='y')
#' polygon(x, y)
#' text(x, y, LETTERS[1:4], pos=4)
#' lines(x[c(1, 3)], y[c(1, 3)], lty='dashed', col='red')
#' @export

quadArea <- function(x, y) {

	euclid <- function(x1, y1, x2, y2) sqrt((x1 - x2)^2 + (y1 - y2)^2)

	# sides of first triangle
	AB <- euclid(x[1], y[1], x[2], y[2])
	BC <- euclid(x[2], y[2], x[3], y[3])
	CA <- euclid(x[3], y[3], x[1], y[1])
	s1 <- 0.5 * (AB + BC + CA)
	
	# sides of second triangle
	CD <- euclid(x[3], y[3], x[4], y[4])
	DA <- euclid(x[4], y[4], x[1], y[1])
	s2 <- 0.5 * (CD + DA + CA)

	# Heron's formula
	area1 <- if (AB == 0 | BC == 0 | CA == 0) {
		0
	} else {
		sqrt(s1 * (s1 - AB) * (s1 - BC) * (s1 - CA))
	}

	area2 <- if (CD == 0 | DA == 0 | CA == 0) {
		0
	} else {
		sqrt(s2 * (s2 - AB) * (s2 - BC) * (s2 - CA))
	}

	area1 + area2

}
