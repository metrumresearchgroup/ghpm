context("test-projectboard")

describe("projectboard functions", {

	test_that("get_projectboard()", {
		result <- get_projectboard("metrumresearchgroup", "pkgr", 2)

		expect_true(length(result) > 0)
		expect_equal(names(result), c("id", "name", "body", "state", "creator", "createdAt", "closedAt"))
		expect_true(is.list(result))
	})

	test_that("get_projectboard_issues()", {
		result <- get_projectboard_issues("metrumresearchgroup", "pkgr")

		expect_true(nrow(result) > 0)
		expect_equal(names(result), c("board", "column", "title", "issue"))
		expect_true(is.data.frame(result))
	})
})
