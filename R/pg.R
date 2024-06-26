#' Download Project Gutenberg metadata for a single work
#'
#' [Project Gutenberg](https://www.gutenberg.org) provides metadata in [RDF
#' format](https://en.wikipedia.org/wiki/Resource_Description_Framework) for
#' each of their works. This function downloads the file for the requested work
#' and transforms it into a tidy data frame.
#'
#' # Not for scraping!
#'
#' Do not use this function to scrape metadata in bulk. Project Gutenberg
#' provides [offline catalog
#' downloads](https://www.gutenberg.org/ebooks/offline_catalogs.html) for that
#' purpose. Be a courteous data scientist and take steps to avoid calling this
#' function repeatedly in your code -- particularly during development. Consider
#' using the [memoise](https://memoise.r-lib.org) package in your code and/or
#' [caching](https://quarto.org/docs/computations/caching.html) in your
#' Rmarkdown and Quarto documents.
#'
#' @param pgid Project Gutenberg ID (i.e.
#'   `https://www.gutenberg.org/ebooks/<PGID>`), a single integer value
#'
#' @return Returns a [tibble][tibble::tibble-package] of RDF triples where
#'   *subject*, *predicate*, and *object* have been renamed to *key*, *name*,
#'   and *value*, discarding the RDF structure in favor of a simpler "long"
#'   format that can be aggregated and reshaped easily through tidyverse
#'   conventions. Note that recursive joins may be required to capture nested
#'   attributes.
#'
#' @references See [A tidyverse lover's intro to
#'   RDF](https://docs.ropensci.org/rdflib/articles/rdf_intro.html) for a great
#'   introduction to the RDF format.
#'
#' @export
pg_metadata <- function(pgid) {
  stopifnot(length(pgid) == 1L && grepl("^\\d+$", pgid))

  url <- sprintf("https://www.gutenberg.org/ebooks/%i.rdf", as.integer(pgid))
  q <- "SELECT ?key ?name ?value WHERE { OPTIONAL { ?key ?name ?value. } }"

  tibble(
    pgid = as.integer(pgid),
    rdflib::rdf_parse(url) |>
      rdflib::rdf_query(q)
  )
}

#' Extract file info from Project Gutenberg metadata
#'
#' @param x metadata as returned by [pg_metadata()]
#'
#' @return Returns a [tibble][tibble::tibble-package] with one row per file
#'   associated with the given Project Gutenberg work.
#'
#' @export
pg_files <- function(x) {
  y <- x |>
    filter(grepl("hasFormat", .data$name)) |>
    transmute(pgid = .data$pgid, key = .data$value) |>
    distinct() |>
    left_join(x, by = c("pgid", "key")) |>
    distinct()

  y <- y |>
    left_join(
      y |>
        filter(grepl("^_:", .data$value)) |>
        transmute(pgid = .data$pgid, format = .data$key, key = .data$value) |>
        left_join(x, by = c("pgid", "key")) |>
        pivot_wider() |>
        transmute(
          pgid = .data$pgid,
          value = .data$key,
          key = .data$format,
          new_value = .data$`http://www.w3.org/1999/02/22-rdf-syntax-ns#value`
        ),
      by = c("pgid", "key", "value")
    ) |>
    transmute(
      pgid = .data$pgid,
      key = .data$key,
      name = .data$name |>
        sub(pattern = "http://purl.org/dc/terms/", replacement = "") |>
        sub(pattern = "http://www.w3.org/1999/02/22-rdf-syntax-ns#", replacement = ""),
      value = coalesce(.data$new_value, .data$value)
    ) |>
    distinct() |>
    pivot_wider(values_fn = list) |>
    transmute(
      pgid = .data$pgid,
      url = .data$key,
      format = map_chr(.data$format, ~ paste(sort(unique(.)), collapse = " + ")),
      size_kb = map_dbl(.data$extent, ~ mean(as.numeric(.))) / 1e3,
      modified = map_vec(.data$modified, ~ max(as.Date(.)))
    ) |>
    arrange(desc(.data$size_kb))

  return(y)
}

#' Download Project Gutenberg content
#'
#' Download (possibly zipped) content from Project Gutenberg to a local
#' directory.
#'
#' @param url URL for Project Gutenberg file
#' @param dest_dir path to save (uncompressed) files
#' @param zip logical indicating if source file is zipped
#'
#' @return Returns path to `dest_dir` invisibly.
#'
#' @export
pg_download <- function(url, dest_dir, zip = FALSE) {
  tmp_dir <- dir_create(file_temp("pg_download-"))
  tmp_pth <- path(tmp_dir, path_file(url))
  on.exit(unlink(tmp_dir, recursive = TRUE))

  download.file(url, tmp_pth)

  if (zip) {
    unzip(tmp_pth, exdir = tmp_dir)
    unlink(tmp_pth)
  }

  dir_copy(tmp_dir, dest_dir, overwrite = TRUE)
}
