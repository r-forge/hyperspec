context ("make")

test_that("make_geteasytargets works", {
	expect_true (setequal (make_geteasytargets("Makefiles/Makefile-test"),
												 c ("all", "DESCRIPTION")))
})
