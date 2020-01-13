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
#' @param param_list List of strings, in order, specifying the property to extract from the response. For example c(c("repository","pullRequest","commits"))
#' @param ... pass through all args (named) that will be passed to graphql_query()
get_query_results <- function(gql_file, param_list, ...) {
	# fetch response and extract data
	response <- sanitize_response(
			graphql_query(
				gql_file,
				...
			)
		)[[param_list]]
	data <- response$nodes

	# page through if necessary
	while(response$pageInfo$hasPreviousPage){
		response <- sanitize_response(
			graphql_query(
				gql_file,
				cursor = response$pageInfo$startCursor,
				...
			)
		)[[param_list]]
		data <- c(data, response$nodes)
	}

	return(data)
}
