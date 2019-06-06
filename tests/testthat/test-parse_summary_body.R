context("test-parse_summary_body")

test_that("Parse different commit description summaries with multiple colons", {
	expect_equal(parse_summary("fix: some string"), list("type" = "fix", "description" = "some string"))
	expect_equal(parse_summary("fix: some string: some extension:"), list("type" = "fix", "description" = "some string: some extension:"))
	expect_equal(parse_summary("fix"), list("type" = NA_character_, "description" = "fix"))
	expect_equal(parse_summary(": askdfjkasd"), list("type" = NA_character_, "description" = ": askdfjkasd"))
	expect_equal(parse_summary(":::: sadasd :: askdfjkasd"), list("type" = NA_character_, "description" = ":::: sadasd :: askdfjkasd"))
})

test_that("Parse commit summaries with multiple lines", {
	expect_equal(parse_summary("fix: some string \nwith a multi-line comment\n and here is some more."), list("type" = "fix", "description" = "some string \nwith a multi-line comment\n and here is some more."))
	expect_equal(parse_summary("fix: some string \nwith a multi-line comment\n and here is some more."), list("type" = "fix", "description" = "some string \nwith a multi-line comment\n and here is some more."))
})

test_that("Parse commit body with no footer", {
	expect_equal(parse_body("now that TEST multiple repos and TEST only one is set\nthe other will TEST as a default\neg, when setting TEST tult to empty string. Hence\nthe logic needed yrd to handle those yyy"),
				 list("body" = "now that TEST multiple repos and TEST only one is set\nthe other will TEST as a default\neg, when setting TEST tult to empty string. Hence\nthe logic needed yrd to handle those yyy", "footer" = NA_character_))
})

test_that("Parse commit body with multiple paragraphs and no footer", {
	expect_equal(parse_body("print the default source type as wellrepo\nso people can see if the binaries/match up.\n\nFor repiment, doesn't\nnecessarilyh a message like:\n\nTEST packages availrom TEST.COM"),
				 list("body" = "print the default source type as wellrepo\nso people can see if the binaries/match up.\n\nFor repiment, doesn't\nnecessarilyh a message like:\n\nTEST packages availrom TEST.COM", "footer" = NA_character_))
	expect_equal(parse_body("when suggests: TEST globally but false TEST, the suggests would still\n\nbe captured in the dep TEST package never had all requirements\nsatisifed and\nwould not install\n\nnow, for the dep tracking,tall\nthe package\n\n"),
				 list("body" = "when suggests: TEST globally but false TEST, the suggests would still\n\nbe captured in the dep TEST package never had all requirements\nsatisifed and\nwould not install\n\nnow, for the dep tracking,tall\nthe package", "footer" = NA_character_))
})

test_that("Parse commit body with footer", {
	expect_equal(parse_body("to keep unique downloads separate, even if the name is the same\n\nreferences: #122"), list("body" = "to keep unique downloads separate, even if the name is the same", "footer" = "references: #122"))
	expect_equal(parse_body("prevents long TEST ASDAS TTT 222 many repos are TEEST (such as with TEST)\nespecially on slower networks\n\nreferences: #212"),
				 list("body" = "prevents long TEST ASDAS TTT 222 many repos are TEEST (such as with TEST)\nespecially on slower networks", "footer" = "references: #212"))
})

test_that("Parse commit body with multiple paragraphs and footer", {
	expect_equal(parse_body("good for testing multirepos, ctetur adipiscing elit, sed match up well\n\nas onsectetur, adipisci velit, sed qui built\n\nin cases  dolorem ipsum quia dependencies\n\nor reverse nostrum exercitationem helpful\n\nreferences: #122"),
				 list("body" = "good for testing multirepos, ctetur adipiscing elit, sed match up well\n\nas onsectetur, adipisci velit, sed qui built\n\nin cases  dolorem ipsum quia dependencies\n\nor reverse nostrum exercitationem helpful", "footer" = "references: #122"))
	expect_equal(parse_body("\n\nd tempor incididunt ut labore et dolore magna aliqua\n\nas onsectetur, adipisci velit, sed qui built\n\n\n\njust ignore R_LIBS_USER so don't need to worry about overriding.l\n\nreferences: #122, fixes: #1222, red: $959"),
				 list("body" = "d tempor incididunt ut labore et dolore magna aliqua\n\nas onsectetur, adipisci velit, sed qui built\n\njust ignore R_LIBS_USER so don't need to worry about overriding.l", "footer" = "references: #122, fixes: #1222, red: $959"))
})

test_that("Parse commit body with multiple paragraphs and footer", {
	expect_equal(parse_body("good for testing multirepos, ctetur adipiscing elit, sed match up well\n\nas onsectetur, adipisci velit, sed qui built\n\nin cases  dolorem ipsum quia dependencies\n\nor reverse nostrum exercitationem helpful\n\nreferences: #122"),
				 list("body" = "good for testing multirepos, ctetur adipiscing elit, sed match up well\n\nas onsectetur, adipisci velit, sed qui built\n\nin cases  dolorem ipsum quia dependencies\n\nor reverse nostrum exercitationem helpful", "footer" = "references: #122"))
	expect_equal(parse_body("\n\nd tempor incididunt ut labore et dolore magna aliqua\n\nas onsectetur, adipisci velit, sed qui built\n\n\n\njust ignore R_LIBS_USER so don't need to worry about overriding.l\n\nreferences: #122, fixes: #1222, red: $959"),
				 list("body" = "d tempor incididunt ut labore et dolore magna aliqua\n\nas onsectetur, adipisci velit, sed qui built\n\njust ignore R_LIBS_USER so don't need to worry about overriding.l", "footer" = "references: #122, fixes: #1222, red: $959"))
})

test_that("Parse commit footer with no body", {
	expect_equal(parse_body("\n\n\n\nreferences: #122, fix: #486"),
				 list("body" = NA_character_, "footer" = "references: #122, fix: #486"))
	expect_equal(parse_body("\n\n\n\nreferences: #122, fix: #486\nrefact: #444 tempor incididunt ut labore et dolore\n\nR_LIBS_USER so don't need to worry about overri"),
				 list("body" = NA_character_, "footer" = "references: #122, fix: #486\nrefact: #444 tempor incididunt ut labore et dolore\n\nR_LIBS_USER so don't need to worry about overri"))
})

test_that("Parse commit body with multi-line footer", {
	expect_equal(parse_body("e ab illo inventore veritatis et quasi architecto beatae vitae dicta su\n\ns qui ratiimus, omnis voluptas ass\n\nsed quia non numquam eius modi tempora inci\n\nrefactor: #222 Here are some additional comments\nmore commcnets\ntest references: #22"),
				 list("body" = "e ab illo inventore veritatis et quasi architecto beatae vitae dicta su\n\ns qui ratiimus, omnis voluptas ass\n\nsed quia non numquam eius modi tempora inci",
				 	 "footer" = "refactor: #222 Here are some additional comments\nmore commcnets\ntest references: #22"))
})
