library(testthat)
library(ghpm)



if(Sys.getenv("GITHUB_PAT") != ""){
	test_check("ghpm")
} else {
	stop("Please set the environment variable GITHUB_PAT with your personal access token to authorize GHPM")
}
