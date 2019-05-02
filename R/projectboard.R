#' Gets a data frame of the issues and their columns on the project board
#' @inheritParams get_milestones
#' @return A data frame containing the issue | title | column | board of the project boards
#' @importFrom purrr map_df reduce
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @export
get_projectboard <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("projects.graphql", org = org, repo = repo, .api_url = .api_url)$repository$projects$nodes

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
