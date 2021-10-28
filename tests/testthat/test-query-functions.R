context("test-query-functions")

test_that("graphql_query()", {
	skip_if_any()

	# Testing File Name
	expect_error(graphql_query(file = "test.graphql", org = "metrumresearchgroup", repo = "bbr"))
	expect_true(is.list(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "bbr")))

	# Testing Passed Parameters
	expect_true(length(graphql_query(file = "milestones.graphql", val1 = "metrumresearchgroup", val2 = "bbr")$errors) > 0)
	expect_true(length(graphql_query(file = "milestones.graphql", org = "bbr", repo = "metrrumresearchgroup")$errors) > 0)

	# Test api_url
	expect_error(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "bbr", .api_url = "example.com"))
	expect_error(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "bbr", .api_url = "http://githuuuub.com"))
})

test_that("sanitize_response()", {
	skip_if_any()

	expect_error(sanitize_response(graphql_query(file = "issues/issues.graphql", org = "metrumresearchgroup", repo = "bbr", cursor = "asdasdas")))
	expect_true(is.list(sanitize_response(graphql_query(file = "milestones.graphql", org = "metrumresearchgroup", repo = "bbr"))))
})

test_that("get_query_results()", {
	skip_if_any()

	expect_true(is.list(get_query_results(gql_file = "milestones.graphql", param_list = c("repository", "milestones"), org = "metrumresearchgroup", repo = "bbr")))
	expect_error(get_query_results(gql_file = "milestones.graphql", param_list = c("random", "param"), org = "metrumresearchgroup", repo = "bbr"))

	# Testing Pagination
	no_paginate <- get_query_results(
		gql_file="issues/issues.graphql",
		param_list = c("repository", "issues"),
		pages = 1,
		org = "metrumresearchgroup",
		repo = "pkgr",
		.api_url = api_url()
	)

	paginate <- get_query_results(
		gql_file="issues/issues.graphql",
		param_list = c("repository", "issues"),
		pages = NULL,
		org = "metrumresearchgroup",
		repo = "pkgr",
		.api_url = api_url()
	)

	expect_true(length(paginate) > length(no_paginate))
})
