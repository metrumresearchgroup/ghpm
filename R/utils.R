assert_url <- function(.url) {
	checkmate::assert_string(.url, fixed = "http")
}
