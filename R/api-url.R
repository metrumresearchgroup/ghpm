#' construct an api_url
#' @param scheme https or http
#' @param hostname hostname
#' @param graphql return the graphql url, false
#' @details
#'The GraphQL endpoint
#'The REST API v3 has numerous endpoints; the GraphQL API v4 has a single endpoint:
#'
#'	http(s)://[hostname]/api/graphql
#' vs for v3 it is at http(s)://[hostname]/api/v3
#'
#' for github.com https://api.github.com and https://api.github.com/graphql
#'
#' @importFrom checkmate assert_string
#' @export
api_url <- function(
	scheme = getOption("ghpm.scheme", "https"),
	hostname = getOption("ghpm.hostname", "api.github.com"),
	graphql = TRUE) {
	# trim suffix if it exists
    url <- 	gsub("\\/$", "", sprintf("%s://%s", scheme, hostname))

    assert_string(url, fixed = "http")
	if (url != "https://api.github.com") {
		# for github enterprise instances
		url <- sprintf("%s/api", url)
		if (graphql) {
			url <- sprintf("%s/graphql", url)
		} else {
			url <- sprintf("%s/v3", url)
		}
		return(url)
	}
	if (graphql) {
		url <- sprintf("%s/graphql", url)
	}
	return(url)
}
