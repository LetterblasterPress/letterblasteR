library(mockery)

test_that("sample_text() returns sentences or wrapped lines", {
  sentences <- sample_text(42)
  lines <- sample_text(42, wrap = 66)

  expect_type(sentences, "character")
  expect_length(sentences, 42)
  expect_false(any(grepl("^\\s", sentences)))
  expect_false(any(grepl("\\s$", sentences)))
  expect_true(length(unique(purrr::map_int(sentences, nchar))) > 1)
  expect_null(names(sentences))

  expect_type(lines, "character")
  expect_length(lines, 42)
  expect_false(any(grepl("^\\s", lines)))
  expect_false(any(grepl("\\s$", lines)))
  expect_identical(unique(purrr::map_int(lines, nchar)), 66L)
  expect_null(names(lines))
})

test_that("estimate_line_length() sets up tex/pdf resources", {
  mock_lengths <- runif(1e3)

  mock_system <- mock(TRUE)
  stub(estimate_line_length, "system", mock_system)
  mock_pdf_text <- mock(paste0(72.27 * mock_lengths, "pt"))
  stub(estimate_line_length, "pdf_text", mock_pdf_text)

  y <- estimate_line_length("foo")

  expect_equal(y, c(mean = mean(mock_lengths), sd = sd(mock_lengths)))
  expect_called(mock_system, 1)
  expect_called(mock_pdf_text, 1)
})
