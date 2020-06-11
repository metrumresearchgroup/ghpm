globalVariables(c("board", "column", "created_at", "issue", "merged_at", "project", "title", "user"))

#' @name ghpm
#' @title GitHub Project Management
#' @description GHPM is a library written in R that grabs and parses data from a github repository making it easier for users to perform analytics.
#' Using Github's flexible GraphQL API and R's dataframe structure, ghpm returns repository data about issues, milestones, project boards, and
#' pull requests which can be melded together to identify useful analytical information.
#' @param org Name of organization to query
#' @param repo Name of repository to query
#' @param .api_url Optional API url to query. Defaults to the value set by `api_url()`. Usually it's "https://api.github.com/graphql"
#' @param pages Number of pages to paginate and pull data from. Each page will contain upto 100 issues/pullrequests. Defaults to NULL for all pages.
#' @importFrom magrittr %>%
NULL

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
graphql_query <- function(file, ..., .api_url = api_url(), .header = NULL) {
	assert_url(.api_url)
	file <- system.file(file, package = "ghpm")
	query <- readChar(file, file.info(file)$size)

	return(gh("POST ", query = query, variables = compact(list(...)), .send_headers = .header, .api_url = .api_url, .token = get_token(.api_url)))
}

#' Helper function for validating tokens
#' @param .api_url API URL you will be querying
#' @return A Personal Access Token via a system environment variable
get_token <- function(.api_url){
	token <- ifelse(grepl(x = .api_url, "https://api.github.com", fixed = TRUE), "GITHUB_PAT", "GHE_PAT")
	if(Sys.getenv(token) == ""){
		stop("Please set the environment variable ", token, " with your personal access token to authorize GHPM")
	}
	return(Sys.getenv(token))
}
