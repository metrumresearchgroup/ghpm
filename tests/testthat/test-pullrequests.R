# context("test-pull-requests")
#
# describe("pull request functions", {
#
# 	it("can get pull request commits", {
# 		mockery::stub(
# 			get_pull_request_commits,
# 			"graphql_query",
# 			jsonlite::read_json("pullrequests/pull_request_commits_response.json")
# 		)
# 		result <- get_pull_request_commits("test", "test")
# 		expect_equal(names(result), c("oid", "message", "author", "date"))
# 		expect_true(is.data.frame(result))
# 	})
#
# 	it("can get all pull requests", {
# 		mockery::stub(
# 			get_all_pull_requests,
# 			"graphql_query",
# 			jsonlite::read_json("pullrequests/all_pull_requests_response.json")
# 		)
# 		result <- get_all_pull_requests("test", "test")
# 		expect_equal(names(result), c("pullrequest", "title", "author", "body", "milestone", "created_at", "merged_by", "merged_at", "merged_to", "state"))
# 		expect_true(is.data.frame(result))
# 	})
#
# 	it("can get all pull request reviewers", {
# 		mockery::stub(
# 			get_pull_request_reviewers,
# 			"graphql_query",
# 			jsonlite::read_json("pullrequests/pull_request_reviewers_response.json")
# 		)
# 		result <- get_pull_request_reviewers("test", "test")
# 		expect_equal(names(result), c("pullrequest", "reviewer"))
# 		expect_true(is.data.frame(result))
# 	})
#
# 	it("can get all pull request comments", {
# 		mockery::stub(
# 			get_pull_request_comments,
# 			"graphql_query",
# 			jsonlite::read_json("pullrequests/pull_request_comments_response.json")
# 		)
# 		result <- get_pull_request_comments("test", "test", 111)
# 		expect_equal(names(result), c("pullrequest", "author", "body", "created_at"))
# 		expect_true(is.data.frame(result))
# 	})
# })
