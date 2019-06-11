#' Gets a data frame of the issues and their columns on the project board
#' @inheritParams get_milestones
#' @return A data frame containing the issue | title | column | board of the project boards
#' @importFrom purrr map_df reduce
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @export
get_projectboard <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("projects/projects.graphql", org = org, repo = repo, .api_url = .api_url)$repository$projects$nodes

	projects <- map_df(data, function(x){
		result <- reduce(x$columns$nodes, get_projectboard_columns,
						 .init = tibble("issue" = numeric(), "title" = character(), "column" = character(), .rows = 0))
		return(result %>% mutate("board" = x$name))
	})

	return(projects %>% select(board, issue, title, column))
}

#' Helper function for get_projectboard that returns a data frame of containing column information for each issue
#' @param .acc Accumlator Value
#' @param .cv Current Value
#' @return A data frame containing issue | title | column information about an issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate
get_projectboard_columns <- function(.acc, .cv){
	if(length(.cv$cards$nodes)){
		rows <- reduce(.cv$cards$nodes, get_projectboard_issues, .init = tibble("issue" = numeric(), "title" = character(), .rows = 0))
		if (nrow(rows)) {
			.acc <- .acc %>% bind_rows(rows %>% mutate("column" = .cv$name))
		}
	}
	return(.acc)
}

#' Helper function for get_projectboard that returns a data frame of numbers and issues
#' @inheritParams get_projectboard_columns
#' @return A data frame containing issue | title of an issue
#' @importFrom tibble tibble add_row
get_projectboard_issues <- function(.acc, .cv){
	if(!is.null(.cv$content) && length(.cv$content) > 0){
		.acc <- .acc %>% add_row("issue" = .cv$content$number, "title" = .cv$content$title)
	}
	return(.acc)
}

#' Creates a projectboard
#' @inheritParams get_milestones
#' @param name Name of projectboard to create
#' @param body Body of projectboard to create
#' @return The ID of the projectboard that was created
#' @export
create_projectboard <- function(org, repo, name, body = "", .api_url = "https://api.github.com/graphql"){
	repo_id <- graphql_query("repo_info.graphql", org = org, repo = repo, .api_url = .api_url)$repository$id
	return(graphql_query("projects/create_project.graphql", owner = repo_id, name = name, body = body, .api_url = .api_url)$createProject$project$id)
}

#' Clones a projectboard
#' @inheritParams get_milestones
#' @param repo_from Name of repo to clone the projectboard from
#' @param number Number of the projectboard to clone
#' @param repo_to Name of repo to clone the projectboard to
#' @return The ID of the projectboard that was cloned
#' @export
clone_projectboard <- function(org, repo_from, number, repo_to, .api_url = "https://api.github.com/graphql"){
	project_from <- graphql_query("projects/project_info.graphql", org = org, repo = repo_from, number = number, .api_url = .api_url)$repository$project
	repo_id <- graphql_query("repo_info.graphql", org = org, repo = repo_to, .api_url = .api_url)$repository$id

	return(graphql_query("projects/clone_project.graphql", owner = repo_id, source = project_from$id, name = project_from$name, body = project_from$body, .api_url = .api_url)$project$id)
}
