
.onAttach <- function(lib, pkg) {

	ver <- read.dcf(file=system.file('DESCRIPTION', package = pkg), fields = 'Version')
	packageStartupMessage(paste0('This is ', pkg, ' ', ver, '.'))
	packageStartupMessage(paste0('Potential back-incompatible changes starting starting with omnibus 1.2.12:'))
	packageStartupMessage(paste0('* mergeLists() now uses `...` as arguments, not `list1` and `list2`.'))
	
}
