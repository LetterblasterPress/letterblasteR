library(mockery)

## diff_text ###################################################################

test_that("diff_text() returns a diff patch as a character vector", {
  a <- letters
  b <- sub("d", "D", a)
  patch <- diff_text(a, b)
  expect_type(patch, "character")
  expect_identical(patch, c(
    "\033[1mdiff --git a/file b/file\033[m",
    "\033[1mindex 0edb856..ccb7357 100644\033[m",
    "\033[1m--- a/file\033[m",
    "\033[1m+++ b/file\033[m",
    "\033[36m@@ -4 +4 @@\033[m \033[mc\033[m",
    "\033[31md\033[m\033[32mD\033[m"
  ))
})

test_that("diff_text() returns an empty string if no diffs", {
  expect_identical(diff_text(letters, letters), character())
})
