#' Convert document to Pandoc's Markdown
#'
#' @param x path to single file or document text as a character vector
#' @param input_format passed to Pandoc option
#'   [`--from`](https://pandoc.org/MANUAL.html#general-options)
#' @param media_dir passed to Pandoc option
#'   [`--extract-media`](https://pandoc.org/MANUAL.html#reader-options)
#' @param shift_heading_level_by passed to Pandoc option
#'   [`--shift-heading-level-by`](https://pandoc.org/MANUAL.html#reader-options)
#' @param default_image_extension passed to Pandoc option
#'   [`--default-image-extension`](https://pandoc.org/MANUAL.html#reader-options)
#' @param dpi passed to Pandoc option
#'   [`--dpi`](https://pandoc.org/MANUAL.html#general-writer-options)
#' @param ... additional Pandoc options added to the `args` parameter of
#'   [pandoc::pandoc_convert()]
#'
#' @return Returns a character vector of tidy markdown
#'
#' @export
to_tidy_md <- function(
    x,
    input_format,
    # output_path,
    media_dir = NULL,
    shift_heading_level_by = 0L,
    default_image_extension = "jpg",
    dpi = 300,
    ...) {
  stopifnot(is.character(x))

  if (is.null(media_dir)) {
    media_dir <- file_temp()
    on.exit(unlink(media_dir, recursive = TRUE))
  }

  # write character input to a temporary file if needed
  if (length(x) > 1 || !file_exists(x)) {
    f <- file_temp()
    on.exit(unlink(f))
    write(x, f)
    x <- f
  }

  stopifnot(is.character(x) && length(x) == 1L && file_exists(x))

  pandoc::pandoc_convert(
    file = x,
    from = input_format,
    to = "markdown",
    # output = output_path,
    standalone = FALSE,
    args = c(
      "--fail-if-warnings",
      sprintf("--shift-heading-level-by=%i", shift_heading_level_by),
      sprintf("--default-image-extension=%s", default_image_extension),
      sprintf("--extract-media=%s", shQuote(media_dir)),
      sprintf("--dpi=%i", dpi),
      "--wrap=auto",
      "--columns=80",
      "--strip-comments", # Is this the best option here?
      ...
    )
  )
}
