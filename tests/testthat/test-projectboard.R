context("test-projectboard")

describe("projectboard functions", {

	test_that("it can get projectboard info", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("projectboard/projectboard_info_response.json"))
			},
			get_projectboard_info("test", "test")
		)

		expect_true(length(result) > 0)
		expect_equal(names(result), c("id", "name", "body", "state", "creator", "createdAt", "closedAt"))
		expect_true(is.list(result))
	})

	test_that("it can get projectboard data", {
		result <- with_mock(
			graphql_query = function(file, ..., .api_url, .header) {
				return(jsonlite::read_json("projectboard/projectboard_response.json"))
			},
			get_projectboard("test", "test")
		)

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("board", "column", "title", "issue"))
		expect_true(is.data.frame(result))
	})
})
