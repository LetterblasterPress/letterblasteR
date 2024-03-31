test_that("to_tidy_md() throws an error if not passed a character vector", {
  expect_error(to_tidy_md(list()), "is not TRUE")
})

test_that("to_tidy_md() converts to tidy markdown, using temp file if needed", {
  x <- c(
    "",
    "# Heading 1",
    "", "",
    paste(sample(stringr::sentences, 5), collapse = " "),
    "",
    sprintf("*  %s", sample(letters, 5))
  )

  y <- list(
    markdown = pandoc_convert(text = x, to = "markdown", args = "--columns=80"),
    html = pandoc_convert(text = x, to = "html"),
    epub = pandoc_convert(
      text = x, to = "epub", output = file_temp(),
      args = "--metadata title=letterblasteR"
    )
  )

  stopifnot(file_exists(y$epub))
  on.exit(unlink(y$epub))

  expect_identical(to_tidy_md(x, "markdown"), y$markdown)
  expect_identical(to_tidy_md(y$markdown, "markdown"), y$markdown)
  expect_identical(to_tidy_md(y$html, "html"), y$markdown)
  expect_equal(
    ignore_attr = TRUE,
    to_tidy_md(y$epub, "epub"),
    c(
      "[]{#title_page.xhtml}",
      "",
      "[]{#ch001.xhtml}",
      "",
      "::: {#ch001.xhtml#heading-1 .section .level1}",
      y$markdown,
      ":::"
    )
  )
})
