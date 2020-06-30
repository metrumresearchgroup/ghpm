#' Gets info about a specified projectboard
#' @inheritParams ghpm
#' @param number The number of the projectboard to query
#' @return A list containing information about the projectboard
#' @export
get_projectboard <- function(org, repo, number, .api_url = api_url()){
	return(sanitize_response(graphql_query("projectboards/projectboard.graphql", org = org, repo = repo, number = number, .api_url = .api_url))$repository$project)
}

#' Gets a data frame of the issues and their columns on the project board
#' @inheritParams ghpm
#' @return A data frame containing the board | column | title | issue of the projectboards
#' @importFrom purrr map_df reduce
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select everything
#' @export
get_projectboard_issues <- function(org, repo, .api_url = api_url()){
	data <- get_query_results(
		gql_file="projectboards/projectboard_issues.graphql",
		param_list = c("repository", "projects"),
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	if(!length(data)){
		return(tibble("board" = character(),
					  "column" = character(),
					  "title" = character(),
					  "issue" = numeric(),
					  .rows = 0
		))
	}

	projects <- map_df(data, function(x){
		result <- reduce(x$columns$nodes, get_projectboard_columns,
						 .init = tibble("issue" = numeric(), "title" = character(), .rows = 0))
		return(mutate(result, "board" = x$name))
	})

	return(select(projects, board, column, title, everything()))
}

#' Helper function for get_projectboard that returns a data frame of containing column information for each issue
#' @param .acc Accumlator Value
#' @param .cv Current Value
#' @return A data frame containing issue | title | column information about an issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr bind_rows mutate
get_projectboard_columns <- function(.acc, .cv){
	if(length(.cv$cards$nodes)){
		rows <- reduce(.cv$cards$nodes, function(.acc, .cv){
			if(!is.null(.cv$content) && length(.cv$content) > 0){
				.acc <- add_row(.acc, "issue" = .cv$content$number, "title" = .cv$content$title)
			}
			return(.acc)
		}, .init = tibble("issue" = numeric(), "title" = character(), .rows = 0))

		if (nrow(rows)) {
			.acc <- bind_rows(.acc, rows %>% mutate("column" = .cv$name))
		}
	}
	return(.acc)
}

#' Creates a projectboard
#' @inheritParams ghpm
#' @param name Name of projectboard to create
#' @param body Body of projectboard to create. Defaults to "".
#' @param columns Optional parameter to specify a vector of projectboard columns to create. (ie: `column = c('column1', 'column2', 'column3', 'column4')`). Defaults to NULL
#' @return Boolean value if creation was successful.
#' @export
create_projectboard <- function(org, repo, name, body = "", columns = NULL, .api_url = api_url()){
	repo_id <- sanitize_response(graphql_query("repo_info.graphql", org = org, repo = repo, .api_url = .api_url))$repository$id
	proj_id <- sanitize_response(graphql_query("projects/create_project.graphql", owner = repo_id, name = name, body = body, .api_url = .api_url))$createProject$project$id

	if(!is.null(columns)){
		lapply(columns, function(x){
			graphql_query("projects/create_project_column.graphql", owner = proj_id, name = x, .api_url = .api_url)
		})
	}

	return(TRUE)
}

#' Clones a projectboard
#' @inheritParams ghpm
#' @param repo_from Name of repo to clone the projectboard from
#' @param number Number of the projectboard to clone
#' @param repo_to Name of repo to clone the projectboard to
#' @return The ID of the projectboard that was cloned
#' @export
clone_projectboard <- function(org, repo_from, number, repo_to, .api_url = api_url()){
	project_from <- get_projectboard_info(org, repo = repo_from, number, .api_url)$repository$project
	repo_id <- sanitize_response(graphql_query("repo_info.graphql", org = org, repo = repo_to, .api_url = .api_url))$repository$id

	return(sanitize_response(graphql_query("projects/clone_project.graphql", owner = repo_id, source = project_from$id, name = project_from$name, body = project_from$body, .api_url = .api_url))$project$id)
}
