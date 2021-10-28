context("test-misc-functions")

test_that("get_milestones()", {
	skip_if_any()

	result <- get_milestones("metrumresearchgroup", "bbr")

	expect_true(is.data.frame(result) && nrow(result) > 0)
	expect_equal(names(result), c("title", "number", "description", "creator", "state", "url", "created_at", "closed", "closed_at", "due_on"))
})

test_that("get_user_info()", {
	skip_if_any()

	result <- get_user_info("dpastoor")

	expect_true(is.list(result) && result$id == "MDQ6VXNlcjMxOTYzMTM=")
	expect_equal(names(result), c("id", "name"))
})

