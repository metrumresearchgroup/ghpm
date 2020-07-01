#' Using closures to cache result of the ping and not pollute the global namespace
skip_if_offline <- (function(){
	offline <- NA
	function() {
		if (is.na(offline)) {
			offline <<- tryCatch(
				is.na(pingr::ping_port("github.com", count = 1, timeout = 1)),
				error = function(e) TRUE
			)
		}
		if (offline) skip("Can't connect to Github.com")
	}
})()

skip_if_no_token <- function(){
	env <- Sys.getenv("GITHUB_PAT")
	if (is.null(env) || is.na(env) || env == "") {
		skip("Please set the environment variable GITHUB_PAT with your personal access token to authorize GHPM")
	}
}

skip_if_any <- function(){
	skip_if_offline()
	skip_if_no_token()
}
