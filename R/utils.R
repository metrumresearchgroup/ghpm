assert_url <- function(.url) {
	checkmate::assert_string(.url, fixed = "http")
}

#' Sanitizes the response from the `graphql_query()` function.
#' @param response The result of `graphql_query()`
#' @param stop If true, will stop the execution when an error is returned otherwise returns the response.
sanitize_response <- function(response, stop = TRUE){
	if("errors" %in% names(response)){
		if(stop){
			stop(response$errors$message)
		}
		warning(response$errors$message)
	}
	return(response$data)
}


#' Wrapper around the get_ functions to page through results and extract the data from the response
#' @param gql_file The .graphql (with path) for the query
#' @param param_list List of strings, in order, specifying the property to extract from the response. For example c("repository","pullRequest","commits")
#' #' @param pagination_limit Upper limit on number of rows to return. Function will paginate through results as long as this limit is not exceeded. Defaults to NULL for no limit.
#' @param ... pass through all args (named) that will be passed to graphql_query()
get_query_results <- function(gql_file, param_list, pagination_limit=NULL, ...) {
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
	page_through <- check_page_through(pagination_limit, data)
	while((response$pageInfo$hasPreviousPage) & page_through) {
		response <- sanitize_response(
			graphql_query(
				gql_file,
				cursor = response$pageInfo$startCursor,
				...
			)
		)[[param_list]]
		data <- c(data, response$nodes)

		# check whether to continue paging
		page_through <- check_page_through(pagination_limit, data)
	}
	return(data)
}

# Private helper function to parse whether or not get_query_results() should continue paging through results
# @param pagination_limit Upper limit on number of rows to return. Function will return TRUE as long as this limit is not exceeded.
# @param data The results from the API that will be measured against the pagination_limit
check_page_through <- function(pagination_limit, data) {
	if (is.null(pagination_limit)) {
		page_through = TRUE
	} else if (length(data) < pagination_limit) {
		page_through = TRUE
	} else {
		page_through = FALSE
	}
	return (page_through)
}

