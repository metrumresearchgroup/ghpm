context("test-issues")

test_that("Default behavior of get_issues()", {
	mockery::stub(
		get_repo_issues,
		"graphql_query",
		readRDS("issues/get_issues.rds")
	)
	expect_equal(get_repo_issues("some", "value"), readRDS("issues/validate_get_issues.rds"))
})

test_that("Default behavior of get_issue_labels()", {
	mockery::stub(
		get_repo_issue_labels,
		"graphql_query",
		readRDS("issues/issue_labels.rds")
	)
	expect_equal(get_repo_issue_labels("some", "value"), readRDS("issues/validate_issue_labels.rds"))
})

test_that("Default behavior of get_issue_assignees()", {
	mockery::stub(
		get_issue_assignees,
		"graphql_query",
		readRDS("issues/issue_assignees.rds")
	)
	expect_equal(get_issue_assignees("some", "value"), readRDS("issues/validate_issue_assignees.rds"))
})

test_that("Default behavior of get_issue_events()", {
	mockery::stub(
		get_issue_events,
		"graphql_query",
		readRDS("issues/issue_events.rds")
	)
	expect_equal(get_issue_events("some", "value"), readRDS("issues/validate_issue_events.rds"))
})

test_that("Default behavior of get_issue_comments()", {
	mockery::stub(
		get_issue_comments,
		"graphql_query",
		readRDS("issues/issue_comments.rds")
	)
	expect_equal(get_issue_comments("some", "value"), readRDS("issues/validate_issue_comments.rds"))
})
