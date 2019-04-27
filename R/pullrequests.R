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
								 "milestone" = ifelse(is.null(.cv$milestone), NA, .cv$milestone$title),
								 "createdAt" = .cv$createdAt,
								 "mergedBy" = ifelse(is.null(.cv$mergedBy), NA, .cv$mergedBy$login),
								 "mergedAt" = ifelse(is.null(.cv$mergedAt), NA, .cv$mergedAt),
								 "mergedTo" = .cv$baseRefName,
								 "state" = .cv$state)

		return(.acc)
	}, .init = tibble("number" = integer(), "title" = character(), "author" = character(), "body" = character(),
					  "milestone" = character(), "createdAt" = character(), "mergedBy" = character(),
					  "mergedAt" = character(), "mergedTo" = character(), "state" = character(), .rows = 0))

	return(prs)
}

#' Gets a data frame of the comments of all the pull requests of a given repo
#' @inheritParams get_milestones
#' @return A data frame containing the pullrequest | author | body | createdAt of each pull request comment
#' @importFrom purrr keep map_df reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select
#' @export
get_pullrequest_comments <- function(org, repo, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("pullrequests/pullrequest_comments.graphql", org = org, repo = repo, .api_url = .api_url)$repository$pullRequests$nodes
	data <- keep(data, ~length(.x$comments$nodes) > 0)

	comments <- map_df(data, function(x){
		comment_data <- reduce(x$comments$nodes, function(.acc, .cv){
			return(.acc %>% add_row("author" = .cv$author$login, "body" = .cv$bodyText, "createdAt" = .cv$createdAt))
		}, .init = tibble("author" = character(), "body" = character(), "createdAt" = character(), .rows = 0))

		return(comment_data %>% mutate("pullrequest" = x$number))
	})

	return(comments %>% select("pullrequest", "author", "body", "createdAt"))
}
