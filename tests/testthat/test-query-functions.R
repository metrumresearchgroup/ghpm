context("test-query-functions")

describe("graphql_query()", {

	test_that("file name", {
		#graphql_query <- function(file, ..., .api_url = api_url(), .header = NULL) {

		expect_error(graphql_query(file = "test.graphql", org = "metrumresearchgroup", repo = "rbabylon"))
		expect_true(is.list(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "rbabylon")))
	})



	# test_that("improper names ",{
	# 	get
	# 	mockery::stub(
	# 		get_query_results,
	# 		"graphql_query",
	# 		jsonlite::read_json("queries/query_error.json")
	# 	)
	# 	expect_error(get_query_results("test", list("test", "test"), pages = NULL))
	# })
	#
	# test_that("list selection errors are properly triggered",{
	# 	mockery::stub(
	# 		get_query_results,
	# 		"graphql_query",
	# 		jsonlite::read_json("issues/issues_response.json")
	# 	)
	# 	expect_error(get_query_results(
	# 		gql_file="issues/issues.graphql",
	# 		param_list = c("something", "else"),
	# 		org = "testorg",
	# 		repo = "testrepo",
	# 		.api_url = "test",
	# 		pages = NULL
	# 	))
	# })


})
