

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
	tags <- lapply(gh(glue("GET repos/{org}/{repo}/labels"), .token = Sys.getenv(token), .api_url = .api_url), function(x){
		gh(glue("DELETE repos/{org}/{repo}/labels/{x$name}", .token = Sys.getenv(token)), .api_url = .api_url)
		return(x$name)
	})

	return(message("The following tags were deleted: ", paste0(tags, "\n ")))
}
