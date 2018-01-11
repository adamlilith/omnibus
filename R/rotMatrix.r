#' Rotate values in a matrix.
#'
#' This function rotates the values in a matrix by a user-specified number of degrees. In almost all cases some values will fall outside the matrix so they will be discarded.  Cells that have no rotated values will become \code{NA}. Only scquare matrices can be accomodated. In some cases a rotation will cause cells to have no assigned value because no original values fall within them. In these instances the mean value of surrounding cells is assigned to the cells with missing values. If the angle of rotation is too small then no rotation will occur.
#' @param x Object of class \code{matrix}.
#' @param rot Numeric. Number of degrees to rotate matrix. Values represent difference in degrees between "north" (up) and the clockwise direction.
#' @return A matrix.
#' @seealso [base::t()]
#' @examples
#' x <- matrix(1:100, nrow=10)
#' x
#' rotMatrix(x, 90) # 90 degrees to the right
#' rotMatrix(x, 180) # 180 degrees to the right
#' rotMatrix(x, 45) # 45 degrees to the right
#' rotMatrix(x, 7) # slight rotation
#' rotMatrix(x, 5) # no rotatation because angle is too small
#' @export
rotMatrix <- function(
	x,
	rot
) {

	# convert degrees to radians
	rot <- -rot * 2 * pi / 360

	# Euclidian distance between pair of points
	pairDist <- function(x1, y1, x2, y2) { sqrt( (x1-x2)^2 + (y1-y2)^2 ) }

	## create matrices to store coordinates
	xCoords <- matrix(data=rep( 1:ncol(x) - ((ncol(x) + 1)/ 2), nrow(x) ), nrow=nrow(x), ncol=ncol(x), byrow=TRUE )
	yCoords <- matrix(data=rep( ncol(x):1 - ((ncol(x) + 1)/ 2), nrow(x) ), nrow=nrow(x), ncol=ncol(x), byrow=FALSE )

	## get values from start matrix
	coordsValues <- data.frame(origX=c(xCoords), origY=c(yCoords), Value=c(x))

	## get radius
	coordsValues$radius <- pairDist(x1=0, y1=0, x2=coordsValues$origX, y2=coordsValues$origY)

	coordsValues$origTheta <-
		ifelse( coordsValues$origY > 0 & coordsValues$origX > 0, atan(coordsValues$origY / coordsValues$origX), 0 ) +
		ifelse( coordsValues$origY > 0 & coordsValues$origX < 0, pi + atan(coordsValues$origY / coordsValues$origX), 0 ) +
		ifelse( coordsValues$origY < 0 & coordsValues$origX < 0, pi + atan(coordsValues$origY / coordsValues$origX), 0 ) +
		ifelse( coordsValues$origY < 0 & coordsValues$origX > 0, 2 * pi + atan(coordsValues$origY / coordsValues$origX), 0 ) +
		ifelse( coordsValues$origY==0 & coordsValues$origX > 0, 0, 0 ) +
		ifelse( coordsValues$origY > 0 & coordsValues$origX==0, pi/2, 0 ) +
		ifelse( coordsValues$origY==0 & coordsValues$origX < 0, pi, 0 ) +
		ifelse( coordsValues$origY < 0 & coordsValues$origX==0, (3/2) * pi, 0 )

	## get rotated coords (in matrix coordinate system)
	coordsValues$newX <- coordsValues$radius * cos(coordsValues$origTheta + rot)
	coordsValues$newY <- coordsValues$radius * sin(coordsValues$origTheta + rot)

	## shift so they are in matrix coordinate system
	coordsValues$newX <- round( coordsValues$newX + ( (ncol(x) + 1) / 2) )
	coordsValues$newY <- round( ( (ncol(x) + 1) / 2 ) - coordsValues$newY)

	# ## trim coordinates outside raster
	# coordsValues <- coordsValues[coordValues$newX >= 1 & coordValues$newX <= ncol(x) & coordValues$newY >= 1 & coordValues$newY <= nrow(x), ]
	coordsValues <- subset(coordsValues, newX >= 1 & newX <= ncol(x) & newY >= 1 & newY <= nrow(x))

	# ## remove lines with NA
	# coordsValues <- subset( coordsValues, !is.na(coordsValues$newX) & !is.na(coordsValues$newY) )

	## make new matrix
	y <- matrix( data=NA, nrow=nrow(x), ncol=ncol(x) )

	## assign value to new matrix
	for (countValues in 1:nrow(coordsValues)) { # for each new value

		y[ coordsValues$newY[countValues], coordsValues$newX[countValues] ] <- coordsValues$Value[countValues]

	} # for each new value

	## convert NA's to local average (NA's produced when, because of rounding, no value is assigned to a cell)
	origY <- y # remember original rotated raster

	for (countCol in 2:(ncol(y)-1)) {

		for (countRow in 2:(nrow(y)-1)) {

			if ( is.na(y[countRow, countCol]) ) { # if value is NA, take mean of Moore neighborhood

				y[countRow, countCol] <- mean( c( origY[(countRow-1):(countRow+1), (countCol-1):(countCol+1)] ), na.rm=T )

			} else { # if value is NOT NA, take value of this cell

				y[countRow, countCol] <- origY[countRow, countCol]

			}

		}

	}

	y

}

