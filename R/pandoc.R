#' Convert document to Pandoc's Markdown
#'
#' @param x path to single file or document text as a character vector
#' @param input_format passed to Pandoc option
#'   [`--from`](https://pandoc.org/MANUAL.html#general-options)
#' @param to passed to Pandoc option
#'   [`--to`](https://pandoc.org/MANUAL.html#general-options)
#' @param ... additional Pandoc options added to the `args` parameter of
#'   [pandoc::pandoc_convert()]
#'
#' @return Returns a character vector of tidy markdown
#'
#' @examples
#' # TODO --shift-heading-level-by
#' # TODO --default-image-extension
#' # TODO --dpi
#' # TODO --extract-media
#'
#' @export
to_tidy_md <- function(x, input_format, to = "markdown", ...) {
  tmp_dir <- dir_create(file_temp("tidy_md-"))
  tmp_pth_x <- path(tmp_dir, "temp", ext = input_format)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # write character input to a temporary file if needed
  if (length(x) > 1 || !file_exists(x)) {
    write(x, tmp_pth_x)
  } else {
    file_copy(x, tmp_pth_x)
  }

  with_dir(
    tmp_dir,
    as.character(
      pandoc::pandoc_convert(
        file = path_file(tmp_pth_x),
        from = input_format,
        to = to,
        standalone = FALSE,
        args = c(
          "--fail-if-warnings",
          "--epub-title-page=false",
          "--reference-location=section",
          "--columns=80",
          ...
        )
      )
    )
  )
}
