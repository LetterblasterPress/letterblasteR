library(mockery)

## pg_metadata #################################################################

test_that("pg_metadata() returns errors for invalid inputs", {
  expect_error(pg_metadata(pi), "is not TRUE")
  expect_error(pg_metadata("x"), "is not TRUE")
  expect_error(pg_metadata(seq.int(1, 2)), "is not TRUE")
})

test_that("pg_metadat() downloads the expected file and parses it", {
  mock_rdf_parse <- mock("mock RDF", cycle = TRUE)
  stub(pg_metadata, "rdflib::rdf_parse", mock_rdf_parse)
  mock_rdf_query <- mock(iris, cycle = TRUE)
  stub(pg_metadata, "rdflib::rdf_query", mock_rdf_query)

  expect_identical(pg_metadata(42), tibble(pgid = 42L, iris))
  expect_identical(pg_metadata("42"), tibble(pgid = 42L, iris))
  expect_called(mock_rdf_parse, 2)
  expect_args(mock_rdf_parse, 1, "https://www.gutenberg.org/ebooks/42.rdf")
  expect_called(mock_rdf_query, 2)
  expect_args(
    mock_rdf_query, 1,
    "mock RDF",
    "SELECT ?key ?name ?value WHERE { OPTIONAL { ?key ?name ?value. } }"
  )
})

## pg_files ####################################################################

test_that("pg_files() filters and aggregates metadata about each available file", {
  x <- readRDS(test_path("fixtures", "mock_pg_metadata.rds"))
  y <- structure(
    list(
      pgid = c(
        42L, 42L, 42L, 42L, 42L, 42L, 42L, 42L, 42L, 42L, 42L, 42L, 42L, 42L,
        42L, 42L
      ),
      url = c(
        "https://www.gutenberg.org/cache/epub/42/pg42-h.zip",
        "https://www.gutenberg.org/files/42/42-h.zip",
        "https://www.gutenberg.org/ebooks/42.kf8.images",
        "https://www.gutenberg.org/ebooks/42.kindle.images",
        "https://www.gutenberg.org/ebooks/42.epub3.images",
        "https://www.gutenberg.org/ebooks/42.epub.images",
        "https://www.gutenberg.org/ebooks/42.kindle.noimages",
        "https://www.gutenberg.org/files/42/42-h/42-h.htm",
        "https://www.gutenberg.org/ebooks/42.html.images",
        "https://www.gutenberg.org/ebooks/42.txt.utf-8",
        "https://www.gutenberg.org/files/42/42-0.txt",
        "https://www.gutenberg.org/ebooks/42.epub.noimages",
        "https://www.gutenberg.org/files/42/42-0.zip",
        "https://www.gutenberg.org/ebooks/42.rdf",
        "https://www.gutenberg.org/cache/epub/42/pg42.cover.medium.jpg",
        "https://www.gutenberg.org/cache/epub/42/pg42.cover.small.jpg"
      ),
      format = c(
        "application/octet-stream + application/zip",
        "application/octet-stream + application/zip",
        "application/x-mobipocket-ebook",
        "application/x-mobipocket-ebook",
        "application/epub+zip",
        "application/epub+zip",
        "application/x-mobipocket-ebook",
        "text/html",
        "text/html",
        "text/plain + text/plain; charset=us-ascii",
        "text/plain; charset=us-ascii",
        "application/epub+zip",
        "application/octet-stream + application/zip",
        "application/rdf+xml",
        "image/jpeg",
        "image/jpeg"
      ),
      size_kb = c(
        373.68, 373.628, 347.304, 331.824, 293.521, 293.289, 183.136, 179.947,
        175.5765, 164.025, 163.731, 144.305, 63.368, 19.802, 18.361, 2.736
      ),
      modified = structure(
        c(
          19754, 19499, 19754, 19754, 19754, 19754, 19236, 19499, 19754, 19754,
          19499, 19754, 19499, 19754, 19754, 19754
        ),
        class = "Date"
      )
    ),
    row.names = c(NA, -16L),
    class = c(
      "tbl_df",
      "tbl", "data.frame"
    )
  )

  expect_identical(pg_files(x), y)
})

## pg_download_text ############################################################

test_that("pg_download_text() processes (possibly zipped) text in various encodings", {
  # write test files to a temp dir
  tmp_dir <- dir_create(file_temp("pg_text"))
  on.exit(unlink(tmp_dir, recursive = TRUE))

  c("US-ASCII", "ISO_8859-1", "UTF-8") |>
    purrr::map_lgl(function(enc) {
      x <- letters
      Encoding(x) <- enc

      f <- path(tmp_dir, enc)
      f_zip <- fs::path_ext_set(f, "zip")

      con <- file(f, encoding = enc)
      writeLines(x, con)
      close(con)

      zip(f_zip, f)

      return(file_exists(f) & file_exists(f_zip))
    }) |>
    all() |>
    stopifnot()

  # read each test file with pg_download_txt
  stub(pg_download_text, "download.file", fs::file_copy)

  y <- tibble(
    url = dir_ls(tmp_dir),
    zip = grepl("\\.zip$", url),
    encoding = fs::path_ext_remove(fs::path_file(url))
  ) |>
    purrr::pmap(pg_download_text)

  expect_length(y, 6)
  expect_true(all(purrr::map_lgl(y, ~ class(.) == "character")))
  expect_true(all(purrr::map_lgl(y, ~ identical(., letters))))
  expect_true(all(purrr::map_lgl(y, ~ all(Encoding(.) %in% c("unknown", "UTF-8")))))
})

## pg_download_html ############################################################

test_that("pg_download_html() processes (possibly zipped) html in various encodings", {
  # write test files to a temp dir
  tmp_dir <- dir_create(file_temp("pg_html"))
  on.exit(unlink(tmp_dir, recursive = TRUE))

  c("US-ASCII", "ISO_8859-1", "UTF-8") |>
    purrr::map_lgl(function(enc) {
      x <- letters
      Encoding(x) <- enc

      f <- path(tmp_dir, enc)
      f_zip <- fs::path_ext_set(f, "zip")

      con <- file(f, encoding = enc)
      writeLines(x, con)
      close(con)

      zip(f_zip, f)

      return(file_exists(f) & file_exists(f_zip))
    }) |>
    all() |>
    stopifnot()

  # read each test file with pg_download_txt
  stub(pg_download_html, "download.file", fs::file_copy)

  y <- tibble(
    url = dir_ls(tmp_dir),
    zip = grepl("\\.zip$", url),
    encoding = fs::path_ext_remove(fs::path_file(url))
  ) |>
    purrr::pmap(pg_download_html)

  expect_length(y, 6)
  expect_true(all(purrr::map_lgl(y, ~ class(.) == "character")))
  expect_true(all(purrr::map_lgl(y, ~ identical(., letters))))
  expect_true(all(purrr::map_lgl(y, ~ all(Encoding(.) %in% c("unknown", "UTF-8")))))
})
