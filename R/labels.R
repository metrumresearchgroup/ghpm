#' Adds labels from a dataframe to a repository
#' @inheritParams ghpm
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/" This function uses the v3 REST API instead of the v4 GraphQL
#' @param label_df A dataframe containing the columns name, color, description of each label to create. See details.
#' @details The label dataframe should be structured as follows: \code{ data.frame(name = c("Label 1", "Label 2", "Label 3"), color = c("00FFFF", "FFFF00", "FF00FF"), description = c("This is the first description", "The second one", ""))}
#' @return A message showing the tags that were deleted
#' @importFrom glue glue
#' @importFrom purrr pmap
#' @importFrom gh gh
#' @export
create_labels <- function(org, repo, label_df, .api_url = api_url(graphql = FALSE)){
	labels <- pmap(label_df, function(name, color, description){
		gh(glue("POST /repos/{org}/{repo}/labels"),
		   "name" = name,
		   "color" = color,
		   "description" = ifelse(description == "" || is.na(description), "", description),
		   .token = get_token(.api_url),
		   .api_url = .api_url)
		return(name)
	})

	return(message("The following labels were added:\n", paste0(labels, "\n")))
}

#' Deletes all the labels from a given repository.
#' @inheritParams ghpm
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/" This function uses the v3 REST API instead of the v4 GraphQL
#' @return A message showing the tags that were deleted
#' @importFrom glue glue
#' @importFrom gh gh
#' @export
delete_all_labels <- function(org, repo, .api_url = api_url(graphql = FALSE)){
	token <- get_token(.api_url)
	labels <- lapply(gh(glue("GET repos/{org}/{repo}/labels"), .token = token, .api_url = .api_url), function(x){
		gh(glue("DELETE repos/{org}/{repo}/labels/{x$name}"), .token = token, .api_url = .api_url)
		return(x$name)
	})

	return(message("The following labels were deleted:\n", paste0(labels, "\n")))
}
