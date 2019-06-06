context("test-parse_commit")

test_that("Successfully parsed commit with body and no footer", {
	cc <- parse_commit("refactor: pull out plan logic to function", "refactor: pull out plan logic to function\nThis way, can invoke plan elements in install without duplicating code")
	expect_equal(names(cc), c("type", "description", "body", "footer"))
	expect_equal(cc$type, "refactor")
	expect_equal(cc$description, "pull out plan logic to function")
	expect_equal(cc$body, "This way, can invoke plan elements in install without duplicating code")
	expect_equal(cc$footer, NA_character_)
})

test_that("Successfully parsed commit with footer and single reference", {
	cc <- parse_commit("feat: add show-deps flag", "feat: add show-deps flag\nin cases of failure (or curiosity) seeing what the package dependencies\nor reverse dependencies can be helpful\n\nreferences: #11")
	expect_equal(names(cc), c("type", "description", "body", "footer"))
	expect_equal(cc$type, "feat")
	expect_equal(cc$description, "add show-deps flag")
	expect_equal(cc$body, "in cases of failure (or curiosity) seeing what the package dependencies\nor reverse dependencies can be helpful")
	expect_equal(cc$footer, "references: #11")
})

test_that("Successfully parsed commit with multiline body and several footer references", {
	cc <- parse_commit("fix: isolate using --vanilla","fix: isolate using --vanilla\n\nelse, environments with Renviron or Rprofile that set R_LIBS_SITE override the pkgr.yml\nconfiguration causing issues\n\nreferences: #10 #13\nSome additional text\nAnother text blurb")
	expect_equal(names(cc), c("type", "description", "body", "footer"))
	expect_equal(cc$type, "fix")
	expect_equal(cc$description, "isolate using --vanilla")
	expect_equal(cc$body, "else, environments with Renviron or Rprofile that set R_LIBS_SITE override the pkgr.yml\nconfiguration causing issues")
	expect_equal(cc$footer, "references: #10 #13\nSome additional text\nAnother text blurb")
})

test_that("Successfully parsed commit with multiple line breaks in body and footer", {
	cc <- parse_commit("refactor: use platform specific default type and expose repoURL", "refactor: use platform specific default type and expose repoURL\n\nby exposing the repo url, can now create tooling to show\nwhat repo locations downloads are being added to without actually\nrunning through the download functionality.\n\nreferences #17")
	expect_equal(names(cc), c("type", "description", "body", "footer"))
	expect_equal(cc$type, "refactor")
	expect_equal(cc$description, "use platform specific default type and expose repoURL")
	expect_equal(cc$body, "by exposing the repo url, can now create tooling to show\nwhat repo locations downloads are being added to without actually\nrunning through the download functionality.")
	expect_equal(cc$footer, "references #17")
})

test_that("Successfully parsed commit with no body and just footer", {
	cc <- parse_commit("refactor: switch to vanilla, a superset of isolation", "refactor: switch to vanilla, a superset of isolation\n\nfixes: #15\nreferences: #7\n")
	expect_equal(names(cc), c("type", "description", "body", "footer"))
	expect_equal(cc$type, "refactor")
	expect_equal(cc$description, "switch to vanilla, a superset of isolation")
	expect_equal(cc$body, NA_character_)
	expect_equal(cc$footer, "fixes: #15\nreferences: #7")
})
