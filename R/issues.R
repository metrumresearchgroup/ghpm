#' Gets a data frame of the issues associated with a given repo
#' @inheritParams ghpm
#' @return A data frame containing the issue | title | body | creator | milestone | state of each issue
#' @importFrom purrr reduce
#' @importFrom dplyr mutate_at vars arrange
#' @importFrom readr parse_datetime
#' @importFrom tibble tibble add_row
#' @export
get_issues <- function(org, repo, .api_url = api_url(), pages = NULL){
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
								 "closed" = .cv$closed,
								 "closed_at" = .cv$closedAt %||% NA_character_,
								 "resource_path" = .cv$resourcePath,
								 "url" = .cv$url,
								 "last_edited_at" = .cv$lastEditedAt %||% NA_character_,
								 "editor" = ifelse(is.null(.cv$editor), NA_character_, .cv$editor$login),
								 "published_at" = .cv$publishedAt,
								 "creator" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
								 "milestone" = ifelse(is.null(.cv$milestone), NA_character_, .cv$milestone$title),
								 "milestone_number" = ifelse(is.null(.cv$milestone), NA_integer_, .cv$milestone$number),
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("issue" = numeric(),
					  "title" = character(),
					  "body" = character(),
					  "closed" = logical(),
					  "closed_at" = character(),
					  "resource_path" = character(),
					  "url" = character(),
					  "last_edited_at" = character(),
					  "editor" = character(),
					  "published_at" = character(),
					  "creator" = character(),
					  "milestone" = character(),
					  "milestone_number" = integer(),
					  "state" = character(),
					  .rows = 0))

	return(arrange(mutate_at(issues, vars(closed_at, last_edited_at, published_at), parse_datetime), issue))
}

#' Gets a data frame of the labels of each issue
#' @inheritParams ghpm
#' @return A data frame containing issue | label of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything arrange
#' @export
get_issue_labels <- function(org, repo, .api_url = api_url(), pages = NULL){
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
	return(select(arrange(labels, issue), issue, everything()))
}

#' Gets a data frame of the assignees and participants for issues
#' @inheritParams ghpm
#' @return A data frame containing issue number | type (particpant or assignee) | name | login | total count of users of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything bind_rows arrange
#' @export
get_issues_assignees_participants <- function(org, repo, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="issues/issues_assignees_participants.graphql",
		param_list = c("repository", "issues"),
		pages = pages,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	data <- keep(data, ~ (.x$assignees$total_count > 0 || .x$participants$total_count > 0))

	if(!length(data)){
		return(tibble("issue" = numeric(), "type" = character(), "name" = character(),"login" = character(), .rows = 0))
	}
	assignees <- map_df(data, function(x){
		assigndat <- reduce(x$assignees$nodes, function(.acc, .cv){
			if (is.null(.cv)) {
				return(.acc)
			}
			# names are not required and may be null
			return(add_row(.acc, "type" = "assignee", "name" = .cv$name %||% NA_character_, "login" = .cv$login %||% NA_character_))
		}, .init = tibble("type" = character(), "name" = character(),"login" = character(), .rows = 0))
		assigndat$total_count <- x$assignees$total_count

		pardat <- reduce(x$participants$nodes, function(.acc, .cv){
			if (is.null(.cv)) {
				return(.acc)
			}
			return(add_row(.acc, "type" = "participant", "name" = .cv$name %||% NA_character_, "login" = .cv$login %||% NA_character_))
		}, .init = tibble("type" = character(), "name" = character(),"login" = character(), .rows = 0))
		pardat$total_count <- x$participants$total_count
		return(mutate(bind_rows(assigndat, pardat), issue = x$number))
	})
	return(select(arrange(assignees, issue), issue, everything()))
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
			return(add_row(.acc, "project" = .cv$project$name,
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
		return(mutate(event_data, "issue" = x$number))
	})

	return(select(arrange(mutate(timeline, "date" = parse_datetime(date)), project), "issue", everything()))
}

#' Gets a data frame of the last 30 comments of each issue
#' @inheritParams ghpm
#' @return A data frame containing the issue | n_comments | author | publishedAt | lastEditedAt | editor | url | bodyText | of each issue. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce map_df keep
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate_at vars arrange
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

	data <- keep(data, ~.x$comments$n_comments > 0)

	if(!length(data)){
		return(tibble("issue" = numeric(),
					  "n_comments" = numeric(),
					  "author" = character(),
					  "publishedAt" = character(),
					  "lastEditedAt" = character(),
					  "editor" = character(),
					  "url" = character(),
					  "bodyText" = character(),
					  .rows = 0
		))
	}

	comments <- map_df(data, function(x){
		return(reduce(x$comments$nodes, function(.acc, .cv){
			return(add_row(.acc,
						   "issue" = x$number,
						   "n_comments" = x$comments$n_comments,
						   "author" = .cv$author$login %||% NA_character_,
						   "publishedAt" = .cv$publishedAt %||% NA_character_,
						   "lastEditedAt" = .cv$lastEditedAt %||% NA_character_,
						   "editor" = .cv$editor$login %||% NA_character_,
						   "url" = .cv$url,
						   "bodyText" = .cv$bodyText
			))

		}, .init = tibble("issue" = numeric(),
						  "n_comments" = numeric(),
						  "author" = character(),
						  "publishedAt" = character(),
						  "lastEditedAt" = character(),
						  "editor" = character(),
						  "url" = character(),
						  "bodyText" = character()
		)))
	})

	return(arrange(mutate_at(comments, vars(publishedAt, lastEditedAt), parse_datetime), issue))
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
