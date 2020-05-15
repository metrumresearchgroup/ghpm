context("test-issues")

describe("issue functions", {

	test_that("it can get repo issues", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("issues/issues_response.json"))
			},
			get_repo_issues("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("issue", "title", "body", "creator", "milestone", "state"))
		expect_true(is.data.frame(result))
	})

	test_that("it can get repo issue labels", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("issues/issue_labels_response.json"))
			},
			get_repo_issue_labels("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("issue", "label"))
		expect_true((is.data.frame(result)))
	})

	test_that("it can get repo issue assignees", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("issues/issue_assignees_response.json"))
			},
			get_issue_assignees("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("issue", "assigned_to"))
		expect_true((is.data.frame(result)))
	})

	test_that("it can get issue events", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("issues/issue_events_response.json"))
			},
			get_issue_events("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("issue", "project", "type", "column", "author", "date"))
		expect_true((is.data.frame(result)))
	})

	test_that("it can get issue comments", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("issues/issue_comments_response.json"))
			},
			get_issue_comments("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("issue", "comment", "author", "date"))
		expect_true((is.data.frame(result)))
	})

	test_that("it can get issues by a specific milestone", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("issues/issue_milestone_response.json"))
			},
			get_issues_from_milestone("test", "test", 5)
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("issue", "title", "body", "creator", "state"))
		expect_true((is.data.frame(result)))
	})
})
