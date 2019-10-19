context("test-projectboard")

describe("projectboard functions", {
	it("can get projectboard info", {
		mockery::stub(
			get_projectboard_info,
			"graphql_query",
			jsonlite::read_json("projectboard/projectboard_info_response.json")
		)
		result <- get_projectboard_info("test", "test")
		expect_equal(names(result), c("id", "name", "body", "state", "creator", "createdAt", "closedAt"))
		expect_true(is.list(result))
	})

	it("can get projectboard data", {
		mockery::stub(
			get_projectboard,
			"graphql_query",
			jsonlite::read_json("projectboard/projectboard_response.json")
		)
		result <- get_projectboard("test", "test")
		expect_equal(names(result), c("board", "column", "title", "issue"))
		expect_true(is.data.frame(result))
	})
})
