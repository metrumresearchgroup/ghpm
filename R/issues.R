#' Gets a data frame of the issues associated with a given repo
#' @inheritParams get_milestones
#' @return A data frame containing the title | body | creator | number | labels | milestone | state of each issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_issues <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issues/issues.graphql", org = org, repo = repo, .api_url = .api_url)$repository$issues$nodes
	issues <- reduce(data, function(.acc, .cv){
		.acc <- .acc %>% add_row("number" = .cv$number,
								 "title" = .cv$title,
								 "body" = .cv$bodyText,
								 "creator" = .cv$author$login,
								 "milestone" = ifelse(is.null(.cv$milestone), NA, .cv$milestone),
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("number" = integer(), "title" = character(), "body" = character(), "creator" = character(), "milestones" = character(), "state" = character(), .rows = 0))

	return(issues)
}

#' Gets a data frame of the labels of each issue
#' @inheritParams get_milestones
#' @return A data frame containing issue | label of each issue
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select
#' @export
get_issue_labels <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issues/issue_labels.graphql", org = org, repo = repo, .api_url = .api_url)$repository$issues$nodes
	data <- keep(data, ~length(.x$labels$nodes) > 0)

	labels <- map_df(data, function(x){
		label_data <- reduce(x$labels$nodes, function(.acc, .cv){
			return(.acc %>% add_row("label" = .cv$name))
		}, .init = tibble("label" = character(), .rows = 0))

		return(label_data %>% mutate("issue" = x$number))
	})
	return(labels %>% select(issue, label))
}

#' Gets a data frame of the assignees of each issue
#' @inheritParams get_milestones
#' @return A data frame containing issue | assignedTo of each issue
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select
#' @export
get_issue_assignees <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issues/issue_assignees.graphql", org = org, repo = repo, .api_url = .api_url)$repository$issues$nodes
	data <- keep(data, ~length(.x$assignees$nodes) > 0)

	assignees <- map_df(data, function(x){
		assignee_data <- reduce(x$assignees$nodes, function(.acc, .cv){
			return(.acc %>% add_row("assignedTo" = .cv$login))
		}, .init = tibble("assignedTo" = character(), .rows = 0))

		return(assignee_data %>% mutate("issue" = x$number))
	})
	return(assignees %>% select(issue, assignedTo))
}

#' Gets a data frame of the project board events of each issue
#' @inheritParams get_milestones
#' @return A data frame containing issue | project | type | column | author | date of each issue
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate arrange
#' @export
get_issue_events <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issues/issue_events.graphql", org = org, repo = repo, .header = c("Accept" = "application/vnd.github.starfox-preview+json"))$repository$issues$nodes
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
#' @export
get_issue_comments <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("issues/issue_comments.graphql", org = org, repo = repo)$repository$issues$nodes
	data <- keep(data, ~length(.x$comments$nodes) > 0)

	comments <- map_df(data, function(x){
		comment_data <- reduce(x$comments$nodes, function(.acc, .cv){
			return(.acc %>% add_row("comment" = .cv$bodyText, "author" = .cv$author$login, "date" = .cv$createdAt))
		}, .init = tibble("comment" = character(), "author" = character(), "date" = character(), .rows = 0))

		return(comment_data %>% mutate("issue" = x$number))
	})

	return(comments)
}
