test_that("to_tidy_md() processes a character vector", {
  expect_identical(
    to_tidy_md(readLines(inst("mocks/mock.txt")), "markdown"),
    readLines(inst("mocks/tidy_mock-noimages.md"))
  )
})

test_that("to_tidy_md() processes a path to a file", {
  expect_identical(
    to_tidy_md(inst("mocks/mock.txt"), "markdown"),
    readLines(inst("mocks/tidy_mock-noimages.md"))
  )
})

test_that("to_tidy_md() processes an html file", {
  expect_identical(
    to_tidy_md(inst("mocks/mock.html"), "html"),
    readLines(inst("mocks/tidy_mock.html.md"))
  )
})

test_that("to_tidy_md() processes an epub file", {
  expect_identical(
    to_tidy_md(inst("mocks/mock.epub"), "epub"),
    readLines(inst("mocks/tidy_mock.epub.md"))
  )
})
