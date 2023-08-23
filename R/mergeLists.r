#' Merge two lists with precedence
#'
#' This function merges two lists to create a single, combined list. If there is a conflict (e.g., two elements have the same name), items in the second list gain preference. Adapted from Stack Overflow (http://stackoverflow.com/questions/13811501/r-merge-lists-with-overwrite-and-recursion).
#' @param list1,list2 Lists.
#' @return A list.
#' @examples
#' list1 <- list(a=1:3, b='Hello world', c=LETTERS[1:3])
#' list2 <- list(x=4, b='Goodbye world', z=letters[1:2])
#' mergeLists(list1, list2)
#' mergeLists(list2, list1)
#' list3 <- list(m=list(n=4:7, o=pi), a=1:5)
#' mergeLists(list1, list3)
#' @export
mergeLists <- function(list1, list2) {

	combineThem <- function(x, list1, list2) {

		if (is.list(list1[[x]]) & is.list(list2[[x]])) {
			mergeLists(list1[[x]], list2[[x]])
		} else if (x %in% names(list2)) {
			list2[[x]]
		} else {
			list1[[x]]
		}

	}

	names1 <- names(list1)
	names2 <- names(list2)
	combinedNames <- unique(c(names2, names1))

	combined <- sapply(combinedNames, combineThem, list1=list1, list2=list2, simplify=FALSE)
	combined
	
}
