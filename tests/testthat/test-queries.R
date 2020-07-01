context("test-query-functions")

describe("query functions", {



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
