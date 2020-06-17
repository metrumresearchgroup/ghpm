#' Gets a data frame of the milestones associated with a given repo.
#' @inheritParams ghpm
#' @return A data frame containing the title | description | state | author | url of each milestone
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @export
get_milestones <- function(org, repo, .api_url = api_url()){
	data <- get_query_results(
		gql_file="milestones.graphql",
		param_list = c("repository", "milestones"),
		org = org,
		repo = repo,
		.api_url = .api_url
	)

	milestones <- reduce(data, function(.acc, .cv){
		# dueOn is a datetime object: 2020-06-10T00:00:00Z
		return(.acc %>%
			   	add_row("title" = .cv$title,
						"number" = .cv$number,
						"description" = ifelse(is.null(.cv$description), NA_character_, .cv$description),
						"creator" = ifelse(is.null(.cv$creator), NA_character_, .cv$creator$login),
						"state" = .cv$state,
						"url" = .cv$url,
						"createdAt" = .cv$createdAt,
						"closed" = .cv$closed,
						"closedAt" = .cv$closedAt,
						"dueOn" = .cv$dueOn
				)
			   )
	}, .init = tibble(
		"title" = character(),
		"number" = numeric(),
		"description" = character(),
		"creator" = character(),
		"state" = character(),
		"url" = character(),
		"createdAt" = character(),
		"closed" = logical(),
		"closedAt" = character(),
		"dueOn" = character(),
		.rows = 0)
	)
	if (nrow(milestones)) {
		milestones <- mutate_at(milestones, c("createdAt", "closedAt", "dueOn"), readr::parse_datetime)
	}
	return(milestones)
}

#' Gets data about a user based on their login name.
#' @param username The user's login name
#' @param .api_url Optional API url to query. Defaults to the value set by `api_url()`. Usually it's "https://api.github.com/graphql"
#' @return A list containing data about the user
get_user_info <- function(username, .api_url = api_url()){
	return(sanitize_response(graphql_query("user_info.graphql", username = username, .api_url = .api_url))$user)
}

#' Assigns a list of users to an object (usually an issue or pull request)
#' @param id The id of the object
#' @param users A list of user IDs
#' @param .api_url Optional API url to query. Defaults to the value set by `api_url()`. Usually it's "https://api.github.com/graphql"
assign_to_object <- function(id, users, .api_url = api_url()){
	return(sanitize_response(graphql_query("user_info.graphql", id = id, userIDs = users , .api_url = .api_url)))
}
