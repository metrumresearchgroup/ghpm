#' Gets all the users in an organization
#' @inheritParams ghpm
#' @return A vector of usernames in an organization
#' @export
get_users_by_org <- function(org, .api_url = api_url()){
	data <- get_query_results(
		gql_file="users/users_in_org.graphql",
		param_list = c("organization", "membersWithRole"),
		org = org,
		.api_url = .api_url
	)

	return(unlist(data, use.names = FALSE))
}

#' Adds users to an organization
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/" This function uses the v3 REST API instead of the v4 GraphQL
#' @param users A vector of usernames to add
#' @param org The organization to invite them to
#' @return A message showing the users that were added to the org
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom gh gh
#' @export
add_users_to_org <- function(users, org, .api_url = api_url(graphql = FALSE)){
	added_users <- walk(users, function(user){
		gh(glue("PUT /orgs/{org}/membership/{user}"),
		   .token = get_token(.api_url),
		   .api_url = .api_url)

		return(user)
	})

	return(message("The following users were invited:\n", paste0(added_users, "\n")))
}
