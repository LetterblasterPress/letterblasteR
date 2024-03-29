library(mockery)

## diff_text ###################################################################

test_that("diff_text() returns a diff patch as a character vector", {
  a <- letters
  b <- sub("d", "D", a)
  patch <- diff_text(a, b)
  expect_type(patch, "character")
  expect_equal(
    patch,
    c(
      "\033[1mdiff --git a/file b/file\033[m",
      "\033[1mindex 0edb856..ccb7357 100644\033[m",
      "\033[1m--- a/file\033[m",
      "\033[1m+++ b/file\033[m",
      "\033[36m@@ -4 +4 @@\033[m \033[mc\033[m",
      "\033[31md\033[m\033[32mD\033[m"
    ),
    ignore_attr = TRUE
  )
})

test_that("diff_text() returns an empty string if no diffs", {
  expect_equal(
    diff_text(letters, letters),
    character(),
    ignore_attr = TRUE
  )
})

test_that("diff_text() returns a `unidiff` object", {
  expect_s3_class(diff_text(letters, letters), "unidiff")
  expect_s3_class(diff_text(letters, sub("d", "D", letters)), "unidiff")
})

## diff_css ####################################################################

test_that("diff_css() returns CSS, passing optional palette", {
  expect_type(diff_css(), "character")
  expect_length(diff_css(), 1)
  expect_match(diff_css(), "\\.ansi-color-1 +\\{ color: #cd3131 \\}")
  expect_match(diff_css("ubuntu"), "\\.ansi-color-1 +\\{ color: #de382b \\}")
})

## diff_to_html ################################################################

test_that("diff_to_html() returns an error if not passed a `unidiff` object", {
  expect_error(diff_to_html("not a unidiff"), "is not TRUE")
})

test_that("diff_to_html() converts the diff to HTML", {
  diff_text(letters, sub("d", "D", letters)) |>
    diff_to_html() |>
    expect_equal(
      HTML(paste(collapse = "\n", c(
        "<span class=\"ansi ansi-bold\">diff --git a/file b/file</span>",
        "index 0edb856..ccb7357 100644",
        "--- a/file",
        "+++ b/file",
        "<span class=\"ansi ansi-color-6\">@@ -4 +4 @@</span> c",
        "<span class=\"ansi ansi-color-1\">d</span><span class=\"ansi ansi-color-2\">D</span>"
      ))),
      ignore_attr = TRUE
    )
})

## view_diff ###################################################################

test_that("view_diff() returns an error if not in an interactive session", {
  mock_interactive <- mock(FALSE)
  stub(view_diff, "interactive", mock_interactive)
  expect_error(view_diff("x"), "interactive")
  expect_called(mock_interactive, 1)
})

test_that("view_diff() calls html_print() with the expected tag list", {
  stub(view_diff, "interactive", TRUE)
  mock_html_print <- mock("rendered")
  stub(view_diff, "html_print", mock_html_print)

  x <- diff_text(letters, sub("d", "D", letters))

  expect_identical(view_diff(x, "eclipse"), "rendered")
  expect_called(mock_html_print, 1)
  expect_args(
    mock_html_print, 1,
    tagList(tags$head(tags$style(diff_css("eclipse"))), diff_to_html(x))
  )
})
