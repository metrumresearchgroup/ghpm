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

