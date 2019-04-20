#' Parses a specified GraphQL Query from the project directory.
#' @param file Query file to execute
#' @param ... Additional variables to pass on to the query
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/graphql"
#' @return The list containing the query result
#' @importFrom gh gh
#' @importFrom purrr compact
graphql_query <- function(file, ..., .api_endpoint = "https://api.github.com/graphql") {
	file <- system.file(file, package = "ghpm")
	query <- readChar(file, file.info(file)$size)
	return(gh("POST /graphql", query = query, variables = compact(list(...)), .api_url = .api_url))
}
