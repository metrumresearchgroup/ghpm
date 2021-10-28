context("test-issues")

is_nonempty_sorted_df <- function(data){
	return(is.data.frame(data) && nrow(data) > 0 && !is.unsorted(data$issue))
}

test_that("get_issues()", {
	skip_if_any()

	result <- get_issues("metrumresearchgroup", "bbr")

	expect_true(is_nonempty_sorted_df(result))
	expect_equal(names(result), c("issue", "title", "body", "closed", "closed_at", "resource_path",
								  "url", "last_edited_at", "editor", "published_at", "creator",
								  "milestone", "milestone_number", "state"))
})

test_that("get_issue_labels()", {
	skip_if_any()

	result <- get_issue_labels("metrumresearchgroup", "bbr")

	expect_true(is_nonempty_sorted_df(result))
	expect_equal(names(result), c("issue", "label"))
})

test_that("get_issues_assignees_participants()", {
	skip_if_any()

	result <- get_issues_assignees_participants("metrumresearchgroup", "bbr")

	expect_true(is_nonempty_sorted_df(result))
	expect_equal(names(result), c("issue", "type", "name", "login", "total_count"))
})

test_that("get_issue_events()", {
	skip_if_any()

	result <- get_issue_events("metrumresearchgroup", "bbr")

	expect_true(is_nonempty_sorted_df(result))
	expect_equal(names(result), c("issue", "project", "type", "column", "author", "date"))
})

test_that("get_issue_comments()", {
	skip_if_any()

	result <- get_issue_comments("metrumresearchgroup", "bbr")

	expect_true(is_nonempty_sorted_df(result))
	expect_equal(names(result), c("issue", "n_comments", "author", "publishedAt", "lastEditedAt", "editor", "url", "bodyText"))
})

test_that("get_issues_from_milestone()", {
	skip_if_any()

	result <- get_issues_from_milestone("metrumresearchgroup", "bbr", 9)

	expect_true(is_nonempty_sorted_df(result))
	expect_equal(names(result), c("issue", "title", "body", "creator", "state"))
})
