context("test-issues")

test_that("Default behavior of get_issue_labels()", {
	mockery::stub(
		get_repo_issue_labels,
		"graphql_query",
		readRDS("issues/issue_labels.rds")
	)
	expect_equal(get_repo_issue_labels("some", "value"), readRDS("issues/validate_issue_labels.rds"))
})
