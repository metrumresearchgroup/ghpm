#' Parses a specified GraphQL Query from the project directory.
#' @param file Query file to execute
#' @param ... Additional variables to pass on to the query
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/graphql"
#' @return The list containing the query result
#' @importFrom gh gh
#' @importFrom purrr compact
#' @export
graphql_query <- function(file, ..., .api_url = "https://api.github.com/graphql") {
	file <- system.file(file, package = "ghpm")
	query <- readChar(file, file.info(file)$size)
	return(gh("POST ", query = query, variables = compact(list(...)), .api_url = .api_url)$data)
}

#' Gets a data frame of the milestones associated with a given repo.
#' @param org Name of organization to query
#' @param repo Name of repository to query
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/graphql"
#' @return A data frame containing the Title | Description | State | Author | Url of each milestone
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_milestones <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("milestones.graphql", org = org, repo = repo, .api_url = .api_url)$repository$milestones$nodes
	milestones <- reduce(data, function(.acc, .cv){
		return(.acc %>% add_row("Title" = .cv$title,
								"Description" = .cv$description,
								"State" = .cv$state,
								"Author" = .cv$creator$login,
								"Url" = .cv$url))
	}, .init = tibble("Title" = NA, "Description" = NA, "State" = NA, "Author" = NA, "Url" = NA))
	return(milestones)
}

#' Gets a data frame of the issues associated with a given repo
#' @inheritParams get_milestones
#' @return A data frame containing the Title | Body | Author | Number | Labels | Milestone | State of each issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
get_issues <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issues.graphql", org = "metrumresearchgroup", repo = "pkgr")$repository$issues$nodes
	issues <- reduce(data, function(.acc, .cv){
		.acc <- .acc %>% add_row("Title" = .cv$title, "Body" = .cv$bodyText, "Author" = .cv$author$login, "Number" = .cv$number, "Labels" = ifelse(!is_empty(.cv$labels$nodes), .cv$labels$nodes[[1]]$name, NA), "Milestone" = ifelse(is.null(.cv$milestone), "None", .cv$milestone), "State" = .cv$state)

		return(.acc)
	}, .init = tibble("Title" = NA, "Body" = NA, "Author" = NA, "Number" = NA, "Labels" = NA, "Milestone" = NA, "State" = NA))

	return(issues)
}

#' Gets a data frame of the comments of each issue
#' @inheritParams get_milestones
#' @return A data frame containing the Issue | Date | Author | Comment of each issue
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate
get_issue_comments <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issue_comments.graphql", org = org, repo = repo)$repository$issues$nodes
	data <- keep(data, ~length(.x$comments$nodes) > 0)

	comments <- map_df(data, function(x){
		comment_data <- reduce(x$comments$nodes, function(.acc, .cv){
			return(.acc %>% add_row("Comment" = .cv$bodyText, "Author" = .cv$author$login, "Date" = .cv$createdAt))
		}, .init = tibble("Comment" = character(), "Author" = character(), "Date" = character(), .rows = 0))

		return(comment_data %>% mutate("Issue" = x$number))
	})
}
