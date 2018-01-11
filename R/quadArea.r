#' Area of a quadrilateral
#'
#' Calculates the area of a quadrilaterial by dividing the quadrilateral into two triangles.
#' @param x Numeric list. \code{x} coordinates of quadrilateral.
#' @param y Numeric list. \code{y} coordinates of quadrilateral.
#' @return Numeric (area of a quadrilateral in same units as \code{x} and \code{y}.
#' @examples
#' x <- c(0, 6, 4, 1)
#' y <- c(0, 1, 7, 4)
#' quadArea(x, y)
#' plot(1, type='n', xlim=c(0, 7), ylim=c(0, 7), xlab='x', ylab='y')
#' polygon(x, y)
#' @export

quadArea <- function(x, y) {

	euclid <- function(x1, y1, x2, y2) sqrt((x1 - x2)^2 + (y1 - y2)^2)

	# sides of first triangle
	AB <- euclid(x[1], y[1], x[2], y[2])
	BC <- euclid(x[2], y[2], x[3], y[3])
	CA <- euclid(x[3], y[3], x[1], y[1])
	cosC <- (AB^2 + BC^2 - CA^2) / (2 * AB * BC)

	if (AB == 0 | BC == 0 | CA == 0 | is.na(acos(cosC))) {
		area1 <- 0
	} else {
		angle <- acos(cosC)
		area1 <- 0.5 * AB * BC * sin(angle)
	}

	# sides of second triangle
	AB <- euclid(x[1], y[1], x[3], y[3])
	BC <- euclid(x[3], y[3], x[4], y[4])
	CA <- euclid(x[4], y[4], x[1], y[1])
		cosC <- (BC^2 + CA^2 - AB^2) / (2 * BC * CA)

	if (AB == 0 | BC == 0 | CA == 0 | is.na(acos(cosC))) {
		area2 <- 0
	} else {
		angle <- acos(cosC)
		area2 <- 0.5 * CA * BC * sin(angle)
	}

	area1 + area2

}
