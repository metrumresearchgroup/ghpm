context("test-issues")

describe("issue functions", {
	test_that("can get repo issues", {
		mockery::stub(
			get_query_results,
			"graphql_query",
			jsonlite::read_json("issues/issues_response.json")
		)
		result <- get_repo_issues("test", "test")
		expect_equal(names(result), c("issue", "title", "body", "creator", "milestone", "state"))
		expect_true(is.data.frame(result))
	})

	test_that("can get repo issue labels", {
		mockery::stub(
			get_repo_issue_labels,
			"graphql_query",
			jsonlite::read_json("issues/issue_labels_response.json")
		)
		result <- get_repo_issue_labels("test", "test")
		expect_equal(names(result), c("issue", "label"))
		expect_true((is.data.frame(result)))
	})

	test_that("can get repo issue assignees", {
		mockery::stub(
			get_issue_assignees,
			"graphql_query",
			jsonlite::read_json("issues/issue_assignees_response.json")
		)
		result <- get_issue_assignees("test", "test")
		expect_equal(names(result), c("issue", "assigned_to"))
		expect_true((is.data.frame(result)))
	})

	test_that("can get issue events", {
		mockery::stub(
			get_issue_events,
			"graphql_query",
			jsonlite::read_json("issues/issue_events_response.json")
		)
		result <- get_issue_events("test", "test")
		expect_equal(names(result), c("issue", "project", "type", "column", "author", "date"))
		expect_true((is.data.frame(result)))
	})

	test_that("can get issue comments", {
		mockery::stub(
			get_issue_comments,
			"graphql_query",
			jsonlite::read_json("issues/issue_comments_response.json")
		)
		result <- get_issue_comments("test", "test")
		expect_equal(names(result), c("issue", "comment", "author", "date"))
		expect_true((is.data.frame(result)))
	})

	test_that("can get issues by a specific milestone", {
		mockery::stub(
			get_issues_from_milestone,
			"graphql_query",
			jsonlite::read_json("issues/issue_milestone_response.json")
		)
		result <- get_issues_from_milestone("test", "test", 5)
		expect_equal(names(result), c("issue", "title", "body", "creator", "state"))
		expect_true((is.data.frame(result)))
	})
})
