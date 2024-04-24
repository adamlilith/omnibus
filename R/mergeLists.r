#' Merge two lists with precedence
#'
#' This function merges two or more lists to create a single, combined list. If two elements in different lists have the same name, items in the later list gain preference (e.g., if there are three lists, then values in the third list gain precedence over items with the same name in the second, and the second has precedence over items in the first).
#'
#' @param ... Two or more lists.
#'
#' @returns A list.
#'
#' @seealso \code{\link{appendLists}}
#'
#' @examples
#'
#' list1 <- list(a=1:3, b='Hello world!', c=LETTERS[1:3])
#' list2 <- list(x=4, b='Goodbye world!', z=letters[1:2])
#' list3 <- list(x=44, b='What up, world?', z=c('_A_', '_Z_'), w = TRUE)
#'
#' mergeLists(list1, list2)
#' mergeLists(list2, list1)
#'
#' mergeLists(list1, list2, list3)
#' mergeLists(list3, list2, list1)
#'
#' @export
mergeLists <- function(...) {

	lists <- list(...)

	for (i in 1:(length(lists) - 1)) {

		# first join
		if (i == 1) {
		
			list1 <- lists[[i]]
			list2 <- lists[[i + 1]]

			names1 <- names(list1)
			names2 <- names(list2)
			combinedNames <- unique(c(names2, names1))

			combined <- sapply(combinedNames, .combineThem, list1 = list1, list2 = list2, simplify = FALSE)

		# subsequent joins
		} else {

			list1 <- combined
			list2 <- lists[[i + 1]]

			names1 <- names(list1)
			names2 <- names(list2)
			combinedNames <- unique(c(names2, names1))

			combined <- sapply(combinedNames, .combineThem, list1 = list1, list2 = list2, simplify = FALSE)

		}

	}
	combined
		
}

.combineThem <- function(x, list1, list2) {

	if (is.list(list1[[x]]) & is.list(list2[[x]])) {
		mergeLists(list1[[x]], list2[[x]])
	} else if (x %in% names(list2)) {
		list2[[x]]
	} else {
		list1[[x]]
	}

}
