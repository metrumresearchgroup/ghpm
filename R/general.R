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
		return(.acc %>% add_row("title" = .cv$title,
								"number" = .cv$number,
								"description" = ifelse(is.null(.cv$description), NA_character_, .cv$description),
								"state" = .cv$state,
								"author" = ifelse(is.null(.cv$author), NA_character_, .cv$author$login),
								"url" = .cv$url))
	}, .init = tibble("title" = character(), "number" = numeric(), "description" = character(), "state" = character(), "author" = character(), "url" = character(), .rows = 0))
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
