#' Aligned rank transform of non-oaramaric data for further analysis using ANOVA
#'
#' This function performs the aligned rank transforms on non-parametric data. This is useful for further analysis using parametric techniques like ANOVA.
#' @param x Data frame.
#' @param factors Character list. Names of columns of \code{x} used to define factors and levels (default is to use all columns except for the first).
#' @param response Character. Names of column of \code{x} that has response variable (default is to use the first column).
#' @param subject NULL or character. Name of column in \code{x} that has the subject variable. If NULL then this is ignored. If specified, residuals are calculated for each cell defined by factors, \emph{not} by subject and factors, but aligning is done using both factors and subject.
#' @param fun Function. Function used to calculate cell centering statistic (the default is to use: \code{mean} with \code{na.rm=TRUE}). The function can be any that handles a list of one or more elements.
#' @param verbose Logical. If TRUE then display progress.
#' @return Data frame.
#' @details The function successfully re-creates rankings given by \strong{ARTool} (Wobbrock et al. 2011) of data in Higgins et al. (1990) for data with 2 and 3 factors.
#' If \code{response} is ranks and the set of ranks in each cell is the same (e.g., each cell has ranks 1, 2, and 3, but not necessarily in that order), then all values will be equal across the different ART variables.  This occurs because the center of each cell (e.g., the mean) is the same as the grand mean, so the aligned values are simply the residuals. An ANOVA on this data yields no variance across cells, so the F tests are invalid.
#' @references Higgins, J.J., Blair, R.C., and Tashtoush, S.  1990.  The aligned rank transform procedure.  \emph{roceedings of the Conference on Applied Statistics in Agriculture.}  Manhattan, Kansas: Kansas State University, pp. 185-195.
#' @references Peterson, K.  2002.  Six modifications of the aligned rank transform test for interaction. \emph{Journal of Modern Applied Statistical Methods} 1:100-109.
#' @references Wobbrock, J.O., Findlater, L., Gergle, D., and Higgins, J.J.  2011. The aligned rank transform for nonparametric factorial analysis using only ANOVA procedures.  \emph{Proceedings of the ACM Conference on Human Factors in Computing Systems (CHI 2011).} Vancouver, British Columbia (May 7-12, 2011). New York: ACM Press, pp. 143-146.
#' @examples
#' x <- data.frame(
#'    subject=c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c', 'a', 'b', 'c'),
#'    factor1=c('up', 'up', 'up', 'up', 'up', 'up', 'down', 'down', 'down', 'down',
#'       'down', 'down'),
#'    factor2=c('high', 'med', 'low', 'high', 'med', 'low', 'high', 'med', 'low', 'high',
#'       'med', 'low'),
#'    response=c(1, 17, 1, 1, 0, 4, 5, 6, 3, 7, 100, 70)
#' )
#' art(x=x, response='response', factors=c('factor1', 'factor2'))
#' @export

art <- function(
	x,
	response = names(x)[1],
	factors = names(x)[2:ncol(x)],
	subject = NULL,
	fun = function(x) mean(x, na.rm=T),
	verbose = FALSE
) {

	####################
	## pre-processing ##
	####################

	# warnings/errors
	if (length(factors) < 2) stop('Function \"art\" requires two or more factors to be specified.')
	if (length(factors) > 4 | length(factors) > 3 & !is.null(subject)) warning('More than four factors (which may include a subject) have been supplied\nto function \"art\", so align rank transformation will only be calculated\nfor main effects and all 2-, 3-, and 4-way interactions, but not interactions\nof order >4.')

	# remember starting number of columns of x
	startColX <- ncol(x)

	# ensure each factor is actually a factor
	for (thisFactor in factors) x[ , thisFactor] <- as.factor(x[ , thisFactor])

	# get center of response
	grandCenter <- fun(x[ , response])

	##########
	## MAIN ##
	##########

	# get cell labels
	x$cellLabelTEMP <- x[ , factors[1]]
	for (count in 2:length(factors)) x$cellLabelTEMP <- paste(x$cellLabelTEMP, x[ , factors[count]])

	# calculate residuals using JUST factors to define cells
	if (verbose) say('Calculating cell residuals...')
	cellCenter <- stats::aggregate(x[ , response], list(x$cellLabelTEMP), FUN=fun) # cell means
	x$residualsTEMP <- x[ , response] - cellCenter[match(x$cellLabelTEMP, cellCenter$Group.1), 2]

	# add subject as a factor
	if (!is.null(subject)) factors <- c(factors, subject)

	## align MAIN factors
	for (thisFactor in factors) {

		if (verbose) say('Aligning factor ', thisFactor, '...')

		x$TEMP <- NA

		for (thisLevel in levels(x[ , thisFactor])) {

			x$TEMP[which(x[ , thisFactor]==thisLevel)] <- x$residualsTEMP[which(x[ , thisFactor]==thisLevel)] + (fun(x[which(x[ , thisFactor]==thisLevel), response]) - grandCenter)

		}

		names(x)[ncol(x)] <- paste('aligned_', thisFactor, sep='')

	}

	## align two-way interactions
	for (thisFactorONE in factors[1:(length(factors) - 1)]) {

		for (thisFactorTWO in factors[(which(thisFactorONE==factors) + 1):length(factors)]) {

			if (verbose) say('Aligning factors ', thisFactorONE, ' and ', thisFactorTWO, '...')

			x$TEMP <- NA

			for (thisLevelONE in levels(x[ , thisFactorONE])) {

				for (thisLevelTWO in levels(x[ , thisFactorTWO])) {

					x$TEMP[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO)] <-
						x$residualsTEMP[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO)] +
						(
							fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO), response]) -
							fun(x[which(x[ , thisFactorONE]==thisLevelONE), response]) -
							fun(x[which(x[ , thisFactorTWO]==thisLevelTWO), response]) +
							grandCenter
						)

				}

			}

			names(x)[ncol(x)] <- paste('aligned_', thisFactorONE, '_x_', thisFactorTWO, sep='')

		}

	}

	## align three-way interactions
	if (length(factors) > 2) {

		for (thisFactorONE in factors[1:(length(factors) - 2)]) {

			for (thisFactorTWO in factors[(which(thisFactorONE==factors) + 1):(length(factors) - 1)]) {

				for (thisFactorTHREE in factors[(which(thisFactorTWO==factors) + 1):length(factors)]) {

					if (verbose) say('Aligning factors ', thisFactorONE, ', ', thisFactorTWO, ', and ', thisFactorTHREE, '...')

					x$TEMP <- NA

					for (thisLevelONE in levels(x[ , thisFactorONE])) {

						for (thisLevelTWO in levels(x[ , thisFactorTWO])) {

							for (thisLevelTHREE in levels(x[ , thisFactorTHREE])) {

								x$TEMP[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE)] <-
									x$residualsTEMP[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE)] +
									(
										fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE), response]) -
										fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO), response]) -
										fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTHREE]==thisLevelTHREE), response]) -
										fun(x[which(x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE), response]) +
										fun(x[which(x[ , thisFactorONE]==thisLevelONE), response]) +
										fun(x[which(x[ , thisFactorTWO]==thisLevelTWO), response]) +
										fun(x[which(x[ , thisFactorTHREE]==thisLevelTHREE), response]) -
										grandCenter
									)

							}

						}

					}

					names(x)[ncol(x)] <- paste('aligned_', thisFactorONE, '_x_', thisFactorTWO, '_x_', thisFactorTHREE, sep='')

				}

			}

		}

	}

	## align four-way interactions
	if (length(factors) > 3) {

		for (thisFactorONE in factors[1:(length(factors) - 3)]) {

			for (thisFactorTWO in factors[(which(thisFactorONE==factors) + 1):(length(factors) - 2)]) {

				for (thisFactorTHREE in factors[(which(thisFactorTWO==factors) + 1):(length(factors) - 1)]) {

					for (thisFactorFOUR in factors[(which(thisFactorTHREE==factors) + 1):length(factors)]) {

					if (verbose) say('Aligning factors ', thisFactorONE, ', ', thisFactorTWO, ', ', thisFactorTHREE, ', and ', thisFactorFOUR, '...')

						x$TEMP <- NA

						for (thisLevelONE in levels(x[ , thisFactorONE])) {

							for (thisLevelTWO in levels(x[ , thisFactorTWO])) {

								for (thisLevelTHREE in levels(x[ , thisFactorTHREE])) {

									for (thisLevelFOUR in levels(x[ , thisFactorFOUR])) {

										x$TEMP[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE & x[ , thisFactorFOUR]==thisLevelFOUR)] <-

											x$residualsTEMP[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE & x[ , thisFactorFOUR]==thisLevelFOUR)] +
											(
												fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE & x[ , thisFactorFOUR]==thisLevelFOUR), response]) -

												fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE), response]) -
												fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorFOUR]==thisLevelFOUR), response]) -
												fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTHREE]==thisLevelTHREE & x[ , thisFactorFOUR]==thisLevelFOUR), response]) -
												fun(x[which(x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE & x[ , thisFactorFOUR]==thisLevelFOUR), response]) +

												fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTWO]==thisLevelTWO), response]) +
												fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorTHREE]==thisLevelTHREE), response]) +
												fun(x[which(x[ , thisFactorONE]==thisLevelONE & x[ , thisFactorFOUR]==thisLevelFOUR), response]) +
												fun(x[which(x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorTHREE]==thisLevelTHREE), response]) +
												fun(x[which(x[ , thisFactorTWO]==thisLevelTWO & x[ , thisFactorFOUR]==thisLevelFOUR), response]) +
												fun(x[which(x[ , thisFactorTHREE]==thisLevelTHREE & x[ , thisFactorFOUR]==thisLevelFOUR), response]) -

												fun(x[which(x[ , thisFactorONE]==thisLevelONE), response]) -
												fun(x[which(x[ , thisFactorTWO]==thisLevelTWO), response]) -
												fun(x[which(x[ , thisFactorTHREE]==thisLevelTHREE), response]) -
												fun(x[which(x[ , thisFactorFOUR]==thisLevelFOUR), response]) +

												grandCenter
											)

									}

								}

							}

						}

						names(x)[ncol(x)] <- paste('aligned_', thisFactorONE, '_x_', thisFactorTWO, '_x_', thisFactorTHREE, '_x_', thisFactorFOUR, sep='')

					}

				}

			}

		}

	}

	#####################
	## post-processing ##
	#####################

	# remove utility fields
	x$residualsTEMP <- x$cellLabelTEMP <- NULL

	# align ranks
	for (count in (startColX + 1):ncol(x)) {

		x$TEMP <- rank(x[ , count], ties.method='average')
		names(x)[ncol(x)] <- paste('rank_', names(x)[count], sep='')

	}

	x

}
