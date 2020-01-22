assert_url <- function(.url) {
	checkmate::assert_string(.url, fixed = "http")
}

#' Sanitizes the response from the `graphql_query()` function.
#' @param response The result of `graphql_query()`
#' @param stop If true, will stop the execution when an error is returned otherwise returns the response.
sanitize_response <- function(response, stop = TRUE){
	if("errors" %in% names(response)){
		# parse errors
		errors <- character()
		for (i in 1:length(response$errors)) {
			err <- response$errors[[i]]
			errors <- c(errors, paste0("Error ", i, ": ", err$message))
		}
		err_msg <- paste(errors, collapse = "::")
		# return them
		if(stop){
			stop(err_msg)
		}
		warning(err_msg)
	}
	# if no errors, return data
	return(response$data)
}


#' Wrapper around the get_ functions to page through results and extract the data from the response
#' @param gql_file The .graphql (with path) for the query
#' @param param_list List of strings, in order, specifying the property to extract from the response. For example c("repository","pullRequest","commits")
#' @param pages Number of pages to paginate and pull data from. Each page will contain upto 100 issues/pullrequests. Defaults to 1 page.
#' @param ... pass through all args (named) that will be passed to graphql_query()
get_query_results <- function(gql_file, param_list, pages=1, ...) {
	# fetch initial response
	response <- sanitize_response(
			graphql_query(
				gql_file,
				...
			)
		)

	# attempt to extract requested data
	response <- tryCatch(
		response[[param_list]],
		error = function(e) {
			msg <- paste0(
				"Parameter selection failed. Attempted to select `response`$",
				paste(param_list, collapse = "$"),
				" from passed param_list, but `response` does not have those properties. Full error: ",
				e
			)
			stop(msg)
		}
	)
	data <- response$nodes

	# page through if necessary
	while(pages > 1 && (response$pageInfo$hasPreviousPage)) {
		response <- sanitize_response(
			graphql_query(
				gql_file,
				cursor = response$pageInfo$startCursor,
				...
			)
		)[[param_list]]
		data <- c(data, response$nodes)

		# check whether to continue paging
		pages <- pages - 1
	}
	return(data)
}

