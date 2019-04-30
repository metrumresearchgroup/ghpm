context("test-pullrequests")

test_that("Default behavior of get_pullrequests()", {
	mockery::stub(
		get_pullrequests,
		"graphql_query",
		readRDS("pullrequests/get_pullrequests.rds")
	)
	expect_equal(get_pullrequests("some", "value"), readRDS("pullrequests/validated_get_pullrequests.rds"))
})

test_that("Default behavior of get_pullrequest_comments()", {
	mockery::stub(
		get_pullrequest_comments,
		"graphql_query",
		readRDS("pullrequests/pullrequest_comments.rds")
	)
	expect_equal(get_pullrequest_comments("some", "value"), readRDS("pullrequests/validated_pullrequest_comments.rds"))
})

test_that("Default behavior of get_pullrequest_reviewers()", {
	mockery::stub(
		get_pullrequest_reviewers,
		"graphql_query",
		readRDS("pullrequests/pullrequest_reviewers.rds")
	)
	expect_equal(get_pullrequest_reviewers("some", "value"), readRDS("pullrequests/validated_pullrequest_reviewers.rds"))
})

test_that("Default behavior of get_pullrequest_commits()", {
	mockery::stub(
		get_pullrequest_commits,
		"graphql_query",
		readRDS("pullrequests/pullrequest_commits.rds")
	)
	expect_equal(get_pullrequest_commits("some", "value"), readRDS("pullrequests/validated_pullrequest_commits.rds"))
})
