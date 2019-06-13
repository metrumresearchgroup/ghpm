#' Adds labels from a dataframe to a repository
#' @inheritParams get_milestones
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/"]
#' @param label_df A dataframe containing the columns name, color, description of each label to create.
#' ie: \code{ data.frame(name = c("Label 1", "Label 2", "Label 3"), color = c("00FFFF", "FFFF00", "FF00FF"), description = c("This is the first description", "The second one", ""))}
#' @return A message showing the tags that were deleted
#' @importFrom glue glue
#' @importFrom purrr is_empty
#' @importFrom gh gh
#' @export
create_labels <- function(org, repo, label_df, .api_url = "https://api.github.com/"){
	token <- ifelse(.api_url == "https://api.github.com/", "GITHUB_PAT", "GHE_PAT")
	if(Sys.getenv(token) == ""){
		stop(glue("please set the environment variable {token} to authorize"))
	}

	labels <- apply(label_df, 1, function(x){
		gh(glue("POST /repos/{org}/{repo}/labels"),
		   name = x[[1]],
		   color = x[[2]],
		   description = ifelse(is_empty(x[[3]]) || is.na(x[[3]]), "", x[[3]]),
		   .token = Sys.getenv(token))
		return(x[[1]])
	})

	return(message("The following labels were added: \n", paste0(labels, "\n ")))
}

#' Deletes all the labels from a given repository.
#' @inheritParams get_milestones
#' @param .api_url Optional API url to query. Defaults to "https://api.github.com/"
#' @return A message showing the tags that were deleted
#' @importFrom glue glue
#' @importFrom gh gh
#' @export
delete_all_labels <- function(org, repo, .api_url = "https://api.github.com/"){
	token <- ifelse(.api_url == "https://api.github.com/", "GITHUB_PAT", "GHE_PAT")
	if(Sys.getenv(token) == ""){
		stop(glue("please set the environment variable {token} to authorize"))
	}
	labels <- lapply(gh(glue("GET repos/{org}/{repo}/labels"), .token = Sys.getenv(token), .api_url = .api_url), function(x){
		gh(glue("DELETE repos/{org}/{repo}/labels/{x$name}", .token = Sys.getenv(token)), .api_url = .api_url)
		return(x$name)
	})

	return(message("The following labels were deleted:\n", paste0(labels, "\n")))
}
