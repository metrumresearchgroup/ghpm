#' Creates a pull request
#' @inheritParams ghpm
#' @param base Name of base branch that changes will be pulled into
#' @param head Name of head branch where changes will be pulled from
#' @param title Title of pull request
#' @param body Body of pull request. Defaults to ""
#' @param reviewers List of usernames to request reviews from (ie: `reviewers = c('devinp', 'harshb')`)
#' @param assignees List of usernames to assign to the pull request.
#' @return A list containing the title, creation date, author, reviewers, and assignees of the pull request
#' @export
create_pull_request <- function(org, repo, base, head, title, body = "", reviewers = NULL, assignees = NULL, .api_url = api_url()){
	repo_id <- sanitize_response(graphql_query("repo_info.graphql", org = org, repo = repo, .api_url = .api_url))$repository$id

	data <- sanitize_response(
		graphql_query("pullrequests/create_pull_request.graphql",
					  repoID = repo_id,
					  baseBranch = base,
					  headBranch = head,
					  title = title,
					  body = body,
					  .api_url = .api_url))$createPullRequest$pullRequest

	if(!is.null(reviewers) && length(reviewers) > 0){
		userIDs <- lapply(reviewers, function(user){
			return(get_user_info(user, .api_url = .api_url)$id)
		})
		add_pull_request_reviewers(data$id, users = userIDs, .api_url = .api_url)
	}

	if(!is.null(assignees) && length(assignees) > 0){
		userIDs <- lapply(assignees, function(user){
			return(get_user_info(user, .api_url = .api_url)$id)
		})
		assign_to_object(data$id, users = userIDs, .api_url = .api_url)
	}

	return(list(title = data$title, created_at = data$createdAt, author = data$author$login, reviewers = data$reviewRequests$nodes, assignees = data$assignees$nodes))
}

#' Gets a data frame of the pull requests associated with a given repo
#' @inheritParams ghpm
#' @return A data frame containing the pullrequest | title | author | body | milestone | created_at | merged_by | merged_at | merged_to | state of each issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate arrange
#' @importFrom readr parse_datetime
#' @export
get_all_pull_requests <- function(org, repo, .api_url = api_url(), pages = NULL){
	data <- get_query_results(
		gql_file="pullrequests/all_pull_requests.graphql",
		param_list = c("repository", "pullRequests"),
		pages = pages,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	prs <- reduce(data, function(.acc, .cv){
		.acc <- add_row(.acc, "pullrequest" = .cv$number,
								 "title" = .cv$title,
								 "author" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
								 "body" = .cv$body,
								 "milestone" = ifelse(is.null(.cv$milestone), NA_character_, .cv$milestone$title),
								 "created_at" = .cv$createdAt,
								 "merged_by" = ifelse(is.null(.cv$mergedBy), NA_character_, .cv$mergedBy$login),
								 "merged_at" = ifelse(is.null(.cv$mergedAt), NA_character_, .cv$mergedAt),
								 "merged_to" = .cv$baseRefName,
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("pullrequest" = numeric(), "title" = character(), "author" = character(), "body" = character(),
					  "milestone" = character(), "created_at" = character(), "merged_by" = character(),
					  "merged_at" = character(), "merged_to" = character(), "state" = character(), .rows = 0))

	return(arrange(mutate(prs, created_at = parse_datetime(created_at), merged_at = parse_datetime(merged_at)), pullrequest))
}

#' Gets a data frame of the comments of all a pull request
#' @inheritParams ghpm
#' @param number The number of the pullrequest to grab data from.
#' @return A data frame containing the pullrequest | author | body | created_at of each pull request comment. Returns an empty dataframe if none are found.
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate arrange
#' @importFrom readr parse_datetime
#' @export
get_pull_request_comments <- function(org, repo, number, .api_url = api_url()){
	data <- get_query_results(
		gql_file="pullrequests/pull_request_comments.graphql",
		param_list = c("repository", "pullRequest", "comments"),
		number = number,
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	if(!length(data)){
		return(tibble("pullrequest" = number, "author" = character(), "body" = character(), "created_at" = character(), .rows = 0))
	}

	comments <- reduce(data, function(.acc, .cv){
		return(add_row(.acc, "pullrequest" = number,
								"author" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
								"body" = .cv$body,
								"created_at" = .cv$createdAt))
		}, .init = tibble("pullrequest" = numeric(), "author" = character(), "body" = character(), "created_at" = character(), .rows = 0))

	return(arrange(mutate(comments, "created_at" = parse_datetime(created_at)), pullrequest))
}

#' Sets review requests from a specified list of users to a pull request
#' @inheritParams ghpm
#' @param id Pull Request ID
#' @param users A list of users (by username) to add as reviewers from the pull request
#' @return The title and id of the pull request
#' @export
add_pull_request_reviewers <- function(id, users, .api_url = api_url()){
	return(sanitize_response(graphql_query("pullrequests/add_pull_request_reviewers.graphql",
										  id = id, userIDs = user, .api_url = .api_url))$requestReviews$pullRequest)
}

#' Gets a data frame of the reviewers of all the pull requests of a given repo
#' @inheritParams ghpm
#' @return A data frame containing the pullrequest | reviewer. Returns an empty dataframe if none are found.
#' @importFrom purrr keep map_df reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything
#' @export
get_pull_request_reviewers <- function(org, repo, .api_url = api_url()){
	data <- get_query_results(
		gql_file="pullrequests/pull_request_reviewers.graphql",
		param_list = c("repository", "pullRequests"),
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	data <- keep(data, ~length(.x$reviewRequests$nodes) > 0)

	if(!length(data)){
		return(tibble("pullrequest" = numeric(), "reviewer" = character(), .rows = 0))
	}

	reviewers <- map_df(data, function(x){
		review_data <- reduce(x$reviewRequests$nodes, function(.acc, .cv){
			return(add_row(.acc, "reviewer" = ifelse(is.null(.cv$requestedReviewer), NA_character_, .cv$requestedReviewer$login)))
		}, .init = tibble("reviewer" = character(), .rows = 0))

		return(mutate(review_data, "pullrequest" = x$number))
	})

	return(select(reviewers, "pullrequest", everything()))
}
