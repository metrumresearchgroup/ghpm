context("test-users")

test_that("get_users_by_org()", {
	skip_if_any()

	result <- get_users_by_org("metrumresearchgroup")

	expect_true(is.vector(result))
})
