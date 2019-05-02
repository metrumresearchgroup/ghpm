#' Gets a data frame of the commits of all the pull requests of a given repo
#' @inheritParams get_milestones
#' @param number The number of the pullrequest to grab commits from.
#' @param .cc Parse the commits as conventional commits. Defaults to FALSE.
#' @return A data frame containing the oid | message | author | date of each commit of a pull request.
#' If .cc = TRUE, the data frame will contain oid | type | description | body | footer | author | date
#' @importFrom purrr reduce
#' @importFrom tibble tibble add_row
#' @importFrom dplyr mutate select slice bind_cols
#' @importFrom readr parse_datetime
#' @export
get_pullrequest_commits <- function(org, repo, number, .cc = FALSE, .api_url = "https://api.github.com/graphql"){
	data <- graphql_query("pullrequests/pullrequest_commits.graphql", org = org, repo = repo, number = number, .api_url = .api_url)$repository$pullRequest$commits$nodes

	commits <- reduce(data, function(.acc, .cv){
		return(.acc %>% add_row("oid" = .cv$commit$oid,
								"summary" = .cv$commit$messageHeadline,
								"message" = .cv$commit$message,
								"author" = ifelse(is.null(.cv$commit$author), NA_character_, .cv$commit$author$name),
								"date" = .cv$commit$authoredDate))
	}, .init = tibble("oid" = character(), "summary" = character(), "message" = character(), "author" = character(), "date" = character(), .rows = 0)) %>% mutate("date" = parse_datetime(date))

	if(!.cc) return(commits %>% select("oid", "message", "author", "date"))

	cc_commits <- map_df(lapply(1:nrow(commits), function(i) {
		commits %>% select("summary", "message") %>% slice(i) %>% as.list()
	}), process_commit)

	return(commits %>% bind_cols(cc_commits) %>% select("oid", "type", "description", "body", "footer", "author", "date"))
}

#' Parses each commit list object and returns a single row data.frame
#' @param .x a commit object
#' @return 	A data.frame with the following structure type | description | body | footer
#' @importFrom tibble tibble
process_commit <- function(.x){
	parsed_message <- parse_commit(.x$summary, .x$message)
	return(tibble("type" = parsed_message$type,
				  "description" = parsed_message$description,
				  "body" = parsed_message$body,
				  "footer" = parsed_message$footer))
}

#' Parses the commit summary and message
#' @param commit_summary The commit summary
#' @param commit_message commit message
#' @return A list according to spec: type | description | body | footer
#' @importFrom stringr str_replace_all
parse_commit <- function(commit_summary = NULL, commit_message = NULL){
	if (is.null(commit_message) || commit_message == "") {
		return(list("type" = NA_character_, "description" = NA_character_, "body" = NA_character_, "footer" = NA_character_))
	}
	return(append(parse_summary(commit_summary), parse_body(str_replace_all(commit_message, commit_summary, ""))))
}

#' Parses the commit summary which just contains type and description
#' @param commit_summary The commit summary
#' @return A list according to spec: type | description
#' @importFrom stringr str_trim str_split
parse_summary <- function(commit_summary = NULL) {
	summary <- list("type" = NA_character_, "description" = NA_character_)

	if(is.null(commit_summary)){
		return(summary)
	}

	type_desc <- str_trim(str_split(commit_summary, ":", n = 2)[[1]], side = "both")

	if(length(type_desc) > 1 && type_desc[[1]] != ""){
		summary$type <- tolower(type_desc[[1]])
		summary$description <- type_desc[[2]]
	} else {
		summary$description <- commit_summary
	}
	return(summary)
}

#' Parses the commit message which just contains body and footer
#' @param commit_message The commit message
#' @return A list according to the spec body | footer
#' @importFrom stringr str_trim str_split str_detect regex
parse_body <- function(commit_message = NULL){
	message <- list("body" = NA_character_, "footer" = NA_character_)
	if(is.null(commit_message) || commit_message == ""){
		return(message)
	}

	msg <- str_trim(str_split(commit_message, "\n\n")[[1]], side = "both")
	msg <- msg[msg != ""]
	len <- length(msg)
	if (len > 0) {
		if (len > 1 && str_detect(msg[len], regex('^[a-zA-Z]+(:)?\\s+#\\d+'))) {
			message$body <- paste0(msg[1:(len - 1)], collapse = "\n\n")
			message$footer <- msg[len]
		} else if(str_detect(msg[1], regex('^[a-zA-Z]+(:)?\\s+#\\d+'))) {
			message$footer <- paste0(msg[1:len], collapse = "\n\n")
		} else {
			message$body <- paste0(msg[1:len], collapse = "\n\n")
		}
	}
	return(message)
}
