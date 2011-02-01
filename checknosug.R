require <- function (package, ...) {
	package <- as.character (substitute (package))
	(! package %in% pkgSuggests ("hyperSpec")) & 
	base::require (package = package, character.only = TRUE, ...)
}
