## inst ########################################################################

context("inst()")
test_that("it returns the expected path or an empty string", {
  expect_identical(
    inst("foo", "bar"),
    ""
  )

  expect_identical(
    inst("foo", "bar"),
    system.file("foo", "bar", package = "letterblasteR")
  )

  expect_identical(
    inst("WORDLIST"),
    system.file("WORDLIST", package = "letterblasteR")
  )
})
