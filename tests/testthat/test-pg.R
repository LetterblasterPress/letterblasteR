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
