context("test-pull-requests")

describe("pullrequest functions", {

	is_nonempty_sorted_df <- function(data){
		return(is.data.frame(data) && nrow(data) > 0 && !is.unsorted(data$pullrequest))
	}

	test_that("get_pull_request_commits()", {
		result <- get_pull_request_commits("metrumresearchgroup", "rbabylon", 104)
		result_cc <- get_pull_request_commits("metrumresearchgroup", "pkgr", 149, .cc = TRUE)

		expect_true(is.data.frame(result) && nrow(result) > 0)
		expect_equal(names(result), c("oid", "message", "author", "date"))

		expect_true(is.data.frame(result_cc) && nrow(result_cc) > 0)
		expect_equal(names(result_cc), c("oid", "type", "description", "body", "footer", "author", "date"))
	})

	test_that("get_all_pull_requests()", {
		result <- get_all_pull_requests("metrumresearchgroup", "rbabylon")

		expect_true(is_nonempty_sorted_df(result))
		expect_equal(names(result), c("pullrequest", "title", "author", "body", "milestone", "created_at", "merged_by", "merged_at", "merged_to", "state"))
	})

	test_that("get_pull_request_reviewers()", {
		result <- get_pull_request_reviewers("metrumresearchgroup", "rbabylon")

		expect_true(is_nonempty_sorted_df(result))
		expect_equal(names(result), c("pullrequest", "reviewer"))
	})

	test_that("get_pull_request_comments()", {
		result <- get_pull_request_comments("metrumresearchgroup", "rbabylon", 95)

		expect_true(is_nonempty_sorted_df(result))
		expect_equal(names(result), c("pullrequest", "author", "body", "created_at"))
	})
})
