library(mockery)

generate_mock_pg_download <- function(enc = "UTF-8", zip = FALSE, img = FALSE) {
  enc <- match.arg(toupper(enc), iconvlist())

  # generate mock content
  x <- letters
  Encoding(x) <- enc

  # write mock content
  tmp_dir <- dir_create(file_temp("mock_pg_download-"))
  f <- path(tmp_dir, "index", ext = "html")
  con <- file(f, encoding = enc)
  writeLines(x, con)
  close(con)

  # add images
  if (img) {
    img_dir <- dir_create(path(tmp_dir, "images"))
    write("image 1", path(img_dir, "img1"))
  }

  # compress
  if (zip) {
    f <- fs::path_ext_set(f, "zip")

    with_dir(tmp_dir, zip(f, dir_ls(recurse = TRUE)))
  }

  return(f)
}

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

## pg_download_* ###############################################################

test_that("pg_download_text() and pg_download_html() processes (possibly zipped) content in various encodings, with and without images", {
  mocks <- tidyr::expand_grid(
    enc = c("US-ASCII", "ISO_8859-1", "UTF-8"),
    zip = c(FALSE, TRUE),
    img = c(FALSE, TRUE)
  )
  mocks$url <- purrr::pmap_chr(mocks, generate_mock_pg_download)
  on.exit(unlink(fs::path_dir(mocks$url), recursive = TRUE))

  purrr::pmap(mocks, function(enc, zip, img, url) {
    img_dir <- file_temp("images-")
    on.exit(unlink(img_dir, recursive = TRUE))

    stub(pg_download_text, "download.file", fs::file_copy)
    stub(pg_download_html, "download.file", fs::file_copy)

    txt <- pg_download_text(url = url, zip = zip, encoding = enc)
    expect_type(txt, "character")
    expect_identical(txt, letters)
    expect_true(all(Encoding(txt) %in% c("unknown", "UTF-8")))
    rm(txt)

    htm <- pg_download_html(url = url, zip = zip, encoding = enc, image_dir = img_dir)
    expect_type(htm, "character")
    expect_identical(htm, letters)
    expect_true(all(Encoding(htm) %in% c("unknown", "UTF-8")))
    rm(htm)

    if (zip & img) {
      expect_true(dir_exists(img_dir))
      expect_true(fs::file_exists(path(img_dir, "images", "img1")))
    } else {
      expect_false(dir_exists(img_dir))
    }

    return(invisible(NULL))
  })
})

test_that("pg_download_epub() process contentst, with and without images", {
  tmp_dir <- dir_create(file_temp("epub-"))
  qmd_pth <- path(tmp_dir, "mock.qmd")
  epub_pth <- path(tmp_dir, "mock.epub")
  img_dir <- path(tmp_dir, "epub_images")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file_copy(inst("mock.qmd"), qmd_pth)
  quarto::quarto_render(qmd_pth, "epub", execute_dir = tmp_dir)

  stub(pg_download_epub, "download.file", fs::file_copy)

  expect_identical(
    to_tidy_md(epub_pth, "epub") |>
      sub(
        pattern = "!\\[\\]\\((.+)(media/file\\d+\\.png)\\)",
        replacement = "![](\\2)"
      ),
    pg_download_epub(epub_pth, image_dir = img_dir) |>
      sub(
        pattern = "!\\[\\]\\((.+)(media/file\\d+\\.png)\\)",
        replacement = "![](\\2)"
      )
  )
  expect_true(dir_exists(img_dir))
  expect_true(file_exists(path(img_dir, "media", "file0.png")))
})
