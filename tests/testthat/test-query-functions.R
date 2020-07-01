context("test-query-functions")

describe("graphql_query()", {

	test_that("test file name", {
		expect_error(graphql_query(file = "test.graphql", org = "metrumresearchgroup", repo = "rbabylon"))
		expect_true(is.list(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "rbabylon")))
	})

	test_that("test passed paramters", {
		expect_true(length(graphql_query(file = "milestones.graphql", val1 = "metrumresearchgroup", val2 = "rbabylon")$errors) > 0)
		expect_true(length(graphql_query(file = "milestones.graphql", org = "rbabylon", repo = "metrrumresearchgroup")$errors) > 0)
	})

	test_that("test api_url", {
		expect_error(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "rbabylon", .api_url = "example.com"))
		expect_error(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "rbabylon", .api_url = "http://githuuuub.com"))
		expect_true(is.list(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "rbabylon", .api_url = "https://api.github.com/graphql")))
	})
})

describe("sanitize_response()", {

	test_that("properly capturing errors", {
		expect_error(santize_response(graphql_query(file = "test.graphql", org = "metrumresearchgroup", repo = "rbabylon")))
	})
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
