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
  y <- readRDS(test_path("fixtures", "mock_pg_files.rds"))

  expect_identical(pg_files(x), y)
})

## pg_download #################################################################

mock_pg_download <- function(type, zip = FALSE) {
  type <- match.arg(type, c("txt", "html", "epub"))
  tmp_dir <- dir_create(file_temp("mock_pg_download-"))

  x <- inst(path("mocks", "mock", ext = type))
  y <- path(tmp_dir, path_file(x))
  file_copy(x, y)

  if (type == "html") {
    dir_copy(inst("mocks/mock_files"), path(tmp_dir, "mock_files"))
  }

  if (zip) {
    y <- fs::path_ext_set(y, "zip")
    with_dir(tmp_dir, zip(path_file(y), dir_ls(recurse = TRUE)))
  }

  return(y)
}

test_pg_download <- function(type, zip = FALSE) {
  tmp_dir <- dir_create(file_temp("test_pg_download-"))
  mock_url <- mock_pg_download(type = type, zip = zip)
  on.exit(unlink(c(tmp_dir, path_dir(mock_url)), recursive = TRUE))

  stub(pg_download, "download.file", fs::file_copy)
  y <- pg_download(mock_url, tmp_dir, zip = zip)

  expect_true(file_exists(mock_url))
  expect_true(dir_exists(y))
  expect_identical(
    digest::digest(file = dir_ls(y, glob = paste0("*mock.", type))),
    digest::digest(file = inst(path("mocks", "mock", ext = type)))
  )

  if (type == "html" & zip) {
    expect_true(dir_exists(path(tmp_dir, "mock_files")))
  }
}

test_pg_download("txt")
test_pg_download("txt", zip = TRUE)
test_pg_download("html")
test_pg_download("html", zip = TRUE)
test_pg_download("epub")
