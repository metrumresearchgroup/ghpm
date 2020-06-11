context("test-pull-requests")

describe("pull request functions", {

	test_that("it can get pull request commits", {
		result <- with_mock(
			"ghpm::graphql_query" = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("pullrequests/pull_request_commits_response.json"))
			},
			get_pull_request_commits("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("oid", "message", "author", "date"))
		expect_true(is.data.frame(result))
	})

	test_that("it can get all pull requests", {
		result <- with_mock(
			"ghpm::graphql_query" = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("pullrequests/all_pull_requests_response.json"))
			},
			get_all_pull_requests("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("pullrequest", "title", "author", "body", "milestone", "created_at", "merged_by", "merged_at", "merged_to", "state"))
		expect_true(is.data.frame(result))
	})

	test_that("it can get all pull request reviewers", {
		result <- with_mock(
			"ghpm::graphql_query" = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("pullrequests/pull_request_reviewers_response.json"))
			},
			get_pull_request_reviewers("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("pullrequest", "reviewer"))
		expect_true(is.data.frame(result))
	})

	test_that("it can get all pull request comments", {
		result <- with_mock(
			"ghpm::graphql_query" = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("pullrequests/pull_request_comments_response.json"))
			},
			get_pull_request_comments("test", "test", 111)
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("pullrequest", "author", "body", "created_at"))
		expect_true(is.data.frame(result))
	})
})
