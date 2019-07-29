.onLoad <- function(libname, pkgname) {
	op <- options()
	op.ghpm <- list(
		ghpm.api_url = "https://api.github.com"
	)
	toset <- !(names(op.ghpm) %in% names(op))
	if (any(toset)) options(op.ghpm[toset])


	invisible()
}
