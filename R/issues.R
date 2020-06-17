#' Gets a data frame of the issues associated with a given repo
#' @inheritParams ghpm
#' @return A data frame containing the issue | title | body | creator | milestone | state of each issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_repo_issues <- function(org, repo, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="issues/issues.graphql",
		param_list = c("repository", "issues"),
		pages = pages,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	issues <- reduce(data, function(.acc, .cv){
		.acc <- .acc %>% add_row("issue" = .cv$number,
								 "title" = .cv$title,
								 "body" = .cv$body,
								 "creator" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
								 "milestone" = ifelse(is.null(.cv$milestone), NA_character_, .cv$milestone$title),
								 "milestone_number" = ifelse(is.null(.cv$milestone), NA_integer_, .cv$milestone$number),
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("issue" = numeric(),
					  "title" = character(),
					  "body" = character(),
					  "creator" = character(),
					  "milestone" = character(),
					  "milestone_number" = integer(),
					  "state" = character(),
					  .rows = 0))

	return(issues)
}

#' Gets a data frame of the labels of each issue
#' @inheritParams ghpm
#' @return A data frame containing issue | label of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything
#' @export
get_repo_issue_labels <- function(org, repo, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="issues/issue_labels.graphql",
		param_list = c("repository", "issues"),
		pages = pages,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	data <- keep(data, ~length(.x$labels$nodes) > 0)

	if(!length(data)){
		return(tibble("issue" = numeric(), "label" = character(), .rows = 0))
	}

	labels <- map_df(data, function(x){
		label_data <- reduce(x$labels$nodes, function(.acc, .cv){
			return(.acc %>% add_row("label" = .cv$name))
		}, .init = tibble("label" = character(), .rows = 0))

		return(mutate(label_data, "issue" = x$number))
	})
	return(select(labels, issue, everything()))
}

#' Gets a data frame of the assignees of each issue
#' @inheritParams ghpm
#' @return A data frame containing issue | assignedTo of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything
#' @export
get_issue_assignees <- function(org, repo, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="issues/issue_assignees.graphql",
		param_list = c("repository", "issues"),
		pages = pages,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	data <- keep(data, ~length(.x$assignees$nodes) > 0)

	if(!length(data)){
		return(tibble("issue" = numeric(), "assigned_to" = character(), .rows = 0))
	}

	assignees <- map_df(data, function(x){
		assignee_data <- reduce(x$assignees$nodes, function(.acc, .cv){
			return(.acc %>% add_row("assigned_to" = .cv$login))
		}, .init = tibble("assigned_to" = character(), .rows = 0))

		return(assignee_data %>% mutate(issue = x$number))
	})
	return(assignees %>% select(issue, everything()))
}

#' Gets a data frame of the project board events of each issue
#' @inheritParams ghpm
#' @return A data frame containing issue | project | type | column | author | date of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate arrange select everything
#' @importFrom readr parse_datetime
#' @export
get_issue_events <- function(org, repo, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="issues/issue_events.graphql",
		param_list = c("repository", "issues"),
		pages = pages,
		org = org,
		repo = repo,
		.header = c("Accept" = "application/vnd.github.starfox-preview+json"),
		.api_url = .api_url
	)

	data <- keep(data, ~length(.x$timelineItems$nodes) > 0)

	if(!length(data)){
		return(tibble("issue" = numeric(),
					  "project" = character(),
					  "type" = character(),
					  "column" = character(),
					  "author" = character(),
					  "date" = character(),
					  .rows = 0))
	}

	timeline <- map_df(data, function(x){
		event_data <- reduce(x$timelineItems$nodes, function(.acc, .cv){
			return(.acc %>% add_row("project" = .cv$project$name,
									"type" = ifelse(.cv$`__typename` == "AddedToProjectEvent", "Added", "Moved"),
									"column" = .cv$projectColumnName,
									"author" = ifelse(is.null(.cv$actor), NA_character_, .cv$actor$login),
									"date" = .cv$createdAt))

		}, .init = tibble("project" = character(),
						  "type" = character(),
						  "column" = character(),
						  "author" = character(),
						  "date" = character(),
						  .rows = 0))
		return(event_data %>% mutate("issue" = x$number))
	}) %>% mutate("date" = parse_datetime(date))

	return(timeline %>% arrange(project) %>% select("issue", everything()))
}

#' Gets a data frame of the comments of each issue
#' @inheritParams ghpm
#' @return A data frame containing the issue | date | author | comment of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything
#' @importFrom readr parse_datetime
#' @export
get_issue_comments <- function(org, repo, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="issues/issue_comments.graphql",
		param_list = c("repository", "issues"),
		pages = pages,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	data <- keep(data, ~length(.x$comments$nodes) > 0)

	if(!length(data)){
		return(tibble("issue" = numeric(),
					  "comment" = character(),
					  "author" = character(),
					  "date" = character(),
					  .rows = 0))
	}

	comments <- map_df(data, function(x){
		comment_data <- reduce(x$comments$nodes, function(.acc, .cv){
			return(add_row(.acc, "comment" = .cv$body,
									"author" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
									"date" = .cv$createdAt))
		}, .init = tibble("issue" = numeric(),
						  "comment" = character(),
						  "author" = character(),
						  "date" = character(),
						  .rows = 0))

		return(mutate(comment_data, "issue" = x$number))
	})

	comments <- mutate(comments, "date" = parse_datetime(date))

	return(select(comments, "issue", everything()))
}

#' Gets a data frame of the issues of a specific milestone
#' @param milestone The milestone number to query issues from
#' @inheritParams ghpm
#' @return A data frame containing the issue | title | author | body | state of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_issues_from_milestone <- function(org, repo, milestone, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="issues/issue_milestone.graphql",
		param_list = c("repository", "milestone", "issues"),
		milestone = milestone,
		pages = pages,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	issues <- reduce(data, function(.acc, .cv){
		.acc <- add_row(.acc, "issue" = .cv$number,
								 "title" = .cv$title,
								 "body" = .cv$body,
								 "creator" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("issue" = numeric(),
					  "title" = character(),
					  "body" = character(),
					  "creator" = character(),
					  "state" = character(),
					  .rows = 0))

	return(issues)
}
