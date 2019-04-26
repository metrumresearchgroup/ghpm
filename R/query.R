#' Parses a specified GraphQL Query from the project directory.
#' @param file Query file to execute
#' @param ... Additional variables to pass on to the query
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/graphql"
#' @param .header Optional vector of headers to send to query
#' @return The list containing the query result
#' @importFrom gh gh
#' @importFrom purrr compact
#' @export
graphql_query <- function(file, ..., .api_url = "https://api.github.com/graphql", .header = NULL) {
	file <- system.file(file, package = "ghpm")
	query <- readChar(file, file.info(file)$size)
	return(gh("POST ", query = query, variables = compact(list(...)), .api_url = .api_url, .send_headers = .header)$data)
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
								"author" = .cv$creator$login,
								"url" = .cv$url))
	}, .init = tibble("title" = character(), "description" = character(), "state" = character(), "author" = character(), "url" = character(), .rows = 0))
	return(milestones)
}

#' Gets a data frame of the issues associated with a given repo
#' @inheritParams get_milestones
#' @return A data frame containing the title | body | creator | number | labels | milestone | state of each issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_issues <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issues.graphql", org = org, repo = repo, .api_url = .api_url)$repository$issues$nodes
	issues <- reduce(data, function(.acc, .cv){
		.acc <- .acc %>% add_row("title" = .cv$title,
								 "body" = .cv$bodyText,
								 "creator" = .cv$author$login,
								 "number" = .cv$number,
								 "labels" = ifelse(!is_empty(.cv$labels$nodes), .cv$labels$nodes[[1]]$name, NA),
								 "milestone" = ifelse(is.null(.cv$milestone), NA, .cv$milestone),
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("title" = character(), "body" = character(), "creator" = character(), "number" = integer(), "labels" = character(), "milestones" = character(), "state" = character(), .rows = 0))

	return(issues)
}

#' Gets a data frame of the project board events of each issue
#' @inheritParams get_milestones
#' @return A data frame containing issue | project | type | column | author | date of each issue
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate arrange
get_issue_events <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issue_events.graphql", org = org, repo = repo, .header = c("Accept" = "application/vnd.github.starfox-preview+json"))$repository$issues$nodes
	data <- keep(data, ~length(.x$timelineItems$nodes) > 0)

	timeline <- map_df(data, function(x){
		event_data <- reduce(x$timelineItems$nodes, function(.acc, .cv){
			return(.acc %>% add_row("project" = .cv$project$name,
									"type" = ifelse(.cv$`__typename` == "AddedToProjectEvent", "Added", "Moved"),
									"column" = .cv$projectColumnName,
									"author" = .cv$actor$login,
									"date" = .cv$createdAt))

		}, .init = tibble("project" = character(), "type" = character(), "column" = character(), "author" = character(), "date" = character(), .rows = 0))

		return(event_data %>% mutate("Issue" = x$number))
	})

	return(timeline %>% arrange(Project))
}

#' Gets a data frame of the comments of each issue
#' @inheritParams get_milestones
#' @return A data frame containing the issue | date | author | comment of each issue
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate
get_issue_comments <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issue_comments.graphql", org = org, repo = repo)$repository$issues$nodes
	data <- keep(data, ~length(.x$comments$nodes) > 0)

	comments <- map_df(data, function(x){
		comment_data <- reduce(x$comments$nodes, function(.acc, .cv){
			return(.acc %>% add_row("comment" = .cv$bodyText, "author" = .cv$author$login, "date" = .cv$createdAt))
		}, .init = tibble("comment" = character(), "author" = character(), "date" = character(), .rows = 0))

		return(comment_data %>% mutate("issue" = x$number))
	})

	return(comments)
}

#' Gets a data frame of the issues and their columns on the project board
#' @inheritParams get_milestones
#' @return A data frame containing the issue | title | column | board of the project boards
#' @importFrom purrr map_df reduce
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @export
get_projectboard <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("projects.graphql", org = org, repo = repo, .api_url = .api_url)$repository$projects$nodes

	projects <- map_df(data, function(x){
		result <- reduce(x$columns$nodes, get_projectboard_columns,
						 .init = tibble("issue" = numeric(), "title" = character(), "column" = character(), .rows = 0))
		return(result %>% mutate("board" = x$name))
	})

	return(projects)
}

#' Helper function for get_projectboard that returns a data frame of containing column information for each issue
#' @param .acc Accumlator Value
#' @param .cv Current Value
#' @return A data frame containing column | number | title information about an issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate
get_projectboard_columns <- function(.acc, .cv){
	if(length(.cv$cards$nodes)){
		rows <- reduce(.cv$cards$nodes, get_projectboard_issues, .init = tibble("number" = numeric(), "title" = character(), .rows = 0))
		if (nrow(rows)) {
			.acc <- .acc %>% bind_rows(rows %>% mutate("column" = .cv$name))
		}
	}
	return(.acc)
}

#' Helper function for get_projectboard that returns a data frame of numbers and issues
#' @inheritParams get_projectboard_columns
#' @return A data frame containing number | title of an issue
#' @importFrom tibble tibble add_row
get_projectboard_issues <- function(.acc, .cv){
	if(!is.null(.cv$content) && length(.cv$content) > 0){
		.acc <- .acc %>% add_row("number" = .cv$content$number, "title" = .cv$content$title)
	}
	return(.acc)
}
