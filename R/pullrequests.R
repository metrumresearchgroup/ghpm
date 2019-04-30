#' Gets a data frame of the pullrequests associated with a given repo
#' @inheritParams get_milestones
#' @return A data frame containing the number | title | author | body | milestone | createdAt | mergedBy | mergedAt | mergedTo | state of each issue
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_pullrequests <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("pullrequests/pullrequests.graphql", org = org, repo = repo, .api_url = .api_url)$repository$pullRequests$nodes

	prs <- reduce(data, function(.acc, .cv){
		.acc <- .acc %>% add_row("number" = .cv$number,
								 "title" = .cv$title,
								 "author" = .cv$author$login,
								 "body" = .cv$bodyText,
								 "milestone" = ifelse(is.null(.cv$milestone), NA_character_, .cv$milestone$title),
								 "created_at" = .cv$createdAt,
								 "merged_by" = ifelse(is.null(.cv$mergedBy), NA_character_, .cv$mergedBy$login),
								 "merged_at" = ifelse(is.null(.cv$mergedAt), NA_character_, .cv$mergedAt),
								 "merged_to" = .cv$baseRefName,
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("number" = integer(), "title" = character(), "author" = character(), "body" = character(),
					  "milestone" = character(), "created_at" = character(), "merged_by" = character(),
					  "merged_at" = character(), "merged_to" = character(), "state" = character(), .rows = 0))

	return(prs)
}

#' Gets a data frame of the comments of all the pull requests of a given repo
#' @inheritParams get_milestones
#' @return A data frame containing the pullrequest | author | body | createdAt of each pull request comment. Returns an empty dataframe if none are found.
#' @importFrom purrr keep map_df reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything
#' @export
get_pullrequest_comments <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("pullrequests/pullrequest_comments.graphql", org = org, repo = repo, .api_url = .api_url)$repository$pullRequests$nodes
	data <- keep(data, ~length(.x$comments$nodes) > 0)

	if(!length(data)){
		return(tibble("pullrequest" = numeric(), "author" = character(), "body" = character(), "created_at" = character(), .rows = 0))
	}

	comments <- map_df(data, function(x){
		comment_data <- reduce(x$comments$nodes, function(.acc, .cv){
			return(.acc %>% add_row("author" = .cv$author$login, "body" = .cv$bodyText, "created_at" = .cv$createdAt))
		}, .init = tibble("author" = character(), "body" = character(), "created_at" = character(), .rows = 0))

		return(comment_data %>% mutate("pullrequest" = x$number))
	})

	return(comments %>% select("pullrequest", everything()))
}

#' Gets a data frame of the reviewers of all the pull requests of a given repo
#' @inheritParams get_milestones
#' @return A data frame containing the pullrequest | reviewer. Returns an empty dataframe if none are found.
#' @importFrom purrr keep map_df reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select everything
#' @export
get_pullrequest_reviewers <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("pullrequests/pullrequest_reviewers.graphql", org = org, repo = repo, .api_url = .api_url)$repository$pullRequests$nodes
	data <- keep(data, ~length(.x$reviewRequests$nodes) > 0)

	if(!length(data)){
		return(tibble("pullrequest" = numeric(), "reviewer" = character(), .rows = 0))
	}

	reviewers <- map_df(data, function(x){
		review_data <- reduce(x$reviewRequests$nodes, function(.acc, .cv){
			return(.acc %>% add_row("reviewer" = .cv$requestedReviewer$login))
		}, .init = tibble("reviewer" = character(), .rows = 0))

		return(review_data %>% mutate("pullrequest" = x$number))
	})

	return(reviewers %>% select("pullrequest", everything()))
}
