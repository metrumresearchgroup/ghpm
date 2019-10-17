assert_url <- function(.url) {
	checkmate::assert_string(.url, fixed = "http")
}

#' Sanitizes the response from the `graphql_query()` function.
#' @param response The result of `graphql_query()`
#' @param stop If true, will stop the execution when an error is returned otherwise returns the response.
sanitize_respone <- function(response, stop = TRUE){
	if("errors" %in% names(response)){
		if(stop){
			stop(reponse$errors$message)
		}
		warning(response$errors$message)
	}
	return(response$data)
}
