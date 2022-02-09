#' Nicer version of \code{print()} or \code{cat()} function
#'
#' This function is a nicer version of \code{print()} or \code{cat()}, especially when used inline for functions because it displays immediately and pastes all strings together. It also does some rudimentary but optional word wrapping.
#' @param ... character strings to print
#' @param pre Integer >= 0.  Number of blank lines to print before strings
#' @param post Integer >= 0. Number of blank lines to print after strings
#' @param breaks Either \code{NULL}, which causes all strings to be printed on the same line (no wrap overflow) or a positive integer which wraps lines at this character length (e.g., \code{breaks=80} inserts line breaks every 80 characters).
#' @param wiggle Integer >- 0.  Allows line to overrun \code{breaks} length in characters before inserting line breaks.
#' @param preBreak If wrapping long lines indicates how subsequent lines are indented. NULL causes lines to be printed starting at column 1 on the display device. A positive integer inserts \code{preBreak} number of spaces before printing each line. A string causes each line to start with this string.
#' @param level Integer or \code{NULL}. If \code{NULL}, then the items in ... are displayed as-is. Otherwise, a value of 1, 2, or 3 indicates teh heading level, with lower numbers causing more decoration and spacing to be used.
#' @param deco Character. Character to decorate text with if \code{level} is not \code{NULL}.
#' @return Nothing (side effect is output on the display device).
#' @examples
#'
#' say('The quick brown fox ', 'jumps over the lazy ', 'Susan.')
#' say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', breaks=10)
#' say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', level=1)
#' say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', level=2)
#' say('The quick brown fox ', 'jumps over the lazy ', 'Susan.', level=3)
#'
#' @export

say <- function(..., pre = 0, post = 1, breaks = NULL, wiggle = 10, preBreak = 1, level = NULL, deco = '#') {

	x <- paste0(...)
	len <- nchar(x)

	preSpace <- if (!is.null(level)) {
		paste(rep(' ', 3 * (level - 1)), collapse='')
	} else {
		''
	}
	
	# insert line breaks
	if (!is.null(breaks)) {

		if (nchar(x) > breaks + wiggle) {

			numBreaks <- floor(nchar(x) / breaks)

			for (i in 1:floor(nchar(x) / breaks)) {

				start <- substr(x, 1, i * breaks + 2 * (i - 1))

				middle <- if (is.null(preBreak)) {
					'\n'
				} else if (is.numeric(preBreak)) {
					paste('\n', preSpace, paste(rep(' ', preBreak), collapse='', sep=''), collapse='', sep='')
				} else {
					paste('\n', preSpace, preBreak, collapse='', sep='')
				}
				end <- substr(x, i * breaks + 2 * (i - 1) + 1, nchar(x))

				if (nchar(end) > wiggle) x <- paste(start, middle, end, collapse='', sep='')

			}

		}

	}

	# display
	cat(rep('\n', pre + !is.null(level)))
	
	if (!is.null(level)) if (level <= 3) x <- paste(paste(rep(deco, 3), collapse = ''), x, ifelse(level <= 2, paste(rep(deco, 3), collapse = ''), ''))
	
	if (!is.null(level) && level == 1) cat(preSpace, paste(rep(deco, 3 + 1 + len + 1 + 3), collapse=''), '\n')
	cat(preSpace, x)
	if (!is.null(level)) cat('\n')
	if (!is.null(level) && level <= 2) cat(preSpace, paste(rep(deco, 3 + 1 + len + 1 + 3), collapse=''), '\n')
	if (post > 0) cat(paste(rep('\n', post), collapse=''))
	
	utils::flush.console()

}

