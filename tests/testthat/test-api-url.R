describe("api url", {
	it("defaults to graphql github api", {
		expect_equal(api_url(), "https://api.github.com/graphql")
	})
	it("can use the v3 api", {
		expect_equal(api_url(graphql = FALSE), "https://api.github.com")
	})
	it("can use a github enterprise url", {
		.url <- withr::with_options(list("ghpm.hostname" = "ghe.example.com"), {
			list(
				graphql = api_url(),
				v3 = api_url(graphql = FALSE)
			)
		})
		expect_equal(.url$graphql, "https://ghe.example.com/api/graphql")
		expect_equal(.url$v3, "https://ghe.example.com/api/v3")
	})
})
