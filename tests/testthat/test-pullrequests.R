context("test-pullrequests")

test_that("Default behavior of get_all_pull_requests()", {
	mockery::stub(
		get_all_pull_requests,
		"graphql_query",
		readRDS("pullrequests/get_all_pull_requests.rds")
	)
	expect_equal(get_all_pull_requests("some", "value"), readRDS("pullrequests/validated_get_all_pull_requests.rds"))
})

test_that("Default behavior of get_pullrequest_comments()", {
	mockery::stub(
		get_pull_request_comments,
		"graphql_query",
		readRDS("pullrequests/get_pull_request_comments.rds")
	)
	expect_equal(get_pull_request_comments("some", "value", 99), readRDS("pullrequests/validated_get_pull_request_comments.rds"))
})

test_that("Default behavior of get_pull_request_reviewers()", {
	mockery::stub(
		get_pull_request_reviewers,
		"graphql_query",
		readRDS("pullrequests/get_pull_request_reviewers.rds")
	)
	expect_equal(get_pull_request_reviewers("some", "value"), readRDS("pullrequests/validated_get_pull_request_reviewers.rds"))
})

test_that("Default behavior of get_pull_request_commits() with conventional commits DISABLED", {
	mockery::stub(
		get_pull_request_commits,
		"graphql_query",
		readRDS("pullrequests/get_pull_request_commits.rds")
	)
	expect_equal(get_pull_request_commits("some", "value"), readRDS("pullrequests/validated_get_pull_request_commits.rds"))
})

test_that("Default behavior of get_pullrequest_commits() with conventional commits ENABLED", {
	mockery::stub(
		get_pull_request_commits,
		"graphql_query",
		readRDS("pullrequests/get_pull_request_commits.rds")
	)
	expect_equal(get_pull_request_commits("some", "value", .cc = TRUE), readRDS("pullrequests/validated_cc_get_pull_request_commits.rds"))
})
