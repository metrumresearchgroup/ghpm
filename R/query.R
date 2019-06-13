#' Parses a specified GraphQL Query from the project directory.
#' @param file Query file to execute
#' @param ... Additional variables to pass on to the query
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/graphql"
#' @param .header Optional vector of headers to send to query
#' @return The list containing the query result
#' @importFrom gh gh
#' @importFrom purrr compact
#' @importFrom glue glue
#' @export
graphql_query <- function(file, ..., .api_url = "https://api.github.com/graphql", .header = NULL) {
	file <- system.file(file, package = "ghpm")
	query <- readChar(file, file.info(file)$size)

	token <- ifelse(.api_url == "https://api.github.com/graphql", "GITHUB_PAT", "GHE_PAT")
	if(Sys.getenv(token) == ""){
		stop(glue("please set the environment variable {token} to authorize"))
	}

	return(gh("POST ", query = query, variables = compact(list(...)), .send_headers = .header,
			  .api_url = .api_url, .token = Sys.getenv(token))$data)
}

#' Gets a data frame of the milestones associated with a given repo.
#' @param org Name of organization to query
#' @param repo Name of repository to query
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/graphql"
#' @return A data frame containing the title | description | state | author | url of each milestone
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_milestones <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("milestones.graphql", org = org, repo = repo, .api_url = .api_url)$repository$milestones$nodes
	milestones <- reduce(data, function(.acc, .cv){
		return(.acc %>% add_row("title" = .cv$title,
								"description" = .cv$description,
								"state" = .cv$state,
								"author" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
								"url" = .cv$url))
	}, .init = tibble("title" = character(), "description" = character(), "state" = character(), "author" = character(), "url" = character(), .rows = 0))
	return(milestones)
}
