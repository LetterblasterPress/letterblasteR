#' Compare two character vectors
#'
#' This function uses [`git diff`](https://git-scm.com/docs/git-diff) to
#' compare two character vectors.
#'
#' @param a,b character vectors to compare
#'
#' @return Returns a [unified
#'   diff](https://www.gnu.org/software/diffutils/manual/html_node/Example-Unified.html)
#'   as a character vector.
#'
#' @export
#'
#' @examples
#' diff_text(letters, sub("d", "D", letters)) |>
#'   paste(collapse = "\n") |>
#'   write("")
diff_text <- function(a, b) {
  tmp_dir <- dir_create(file_temp("git"))
  tmp_pth <- path(tmp_dir, "file")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  git_init(tmp_dir)
  git_config_set("user.name", "letterblasteR", repo = tmp_dir)
  git_config_set("user.email", "letterblasteR", repo = tmp_dir)

  write(a, tmp_pth)
  git_add("file", repo = tmp_dir)
  git_commit("a", repo = tmp_dir)
  write(b, tmp_pth)

  patch <- with_dir(
    tmp_dir,
    system(
      "git diff -U0 --minimal --color-words",
      intern = TRUE
    )
  )

  structure(patch, class = "unidiff")
}

#' Formatting and viewing diffs
#'
#' These functions prepare diffs for inclusion in HTML documents or viewing
#' in interactive sessions
#'
#' @param x diff, as returned by [diff_text()]
#' @param palette palette name passed to [cli::ansi_html_style()]
#'
#' @return [diff_css()] returns CSS to style diff output, [diff_to_html()]
#'   returns HTML as returned by [htmltools::HTML()], and [view_diff()] renders
#'   the diff in the RStudio Viewer (interactive sessions only).
#'
#' @examples
#' diff_text(letters, sub("d", "D", letters)) |>
#'   diff_to_html() |>
#'   cat()
#'
#' @name diff_helpers
NULL

#' @rdname diff_helpers
#' @export
diff_css <- function(palette = "vscode") {
  ansi_html_style(palette = palette) |>
    format() |>
    paste(collapse = "\n")
}

#' @rdname diff_helpers
#' @export
diff_to_html <- function(x) {
  stopifnot(is(x, "unidiff"))
  ansi_html(x) |>
    paste(collapse = "<br>\n") |>
    HTML()
}

#' @rdname diff_helpers
#' @export
view_diff <- function(x, palette = "vscode") {
  if (!interactive()) {
    stop("`view_diff()` can only be used in interactive sessions")
  }

  tagList(tags$head(tags$style(diff_css(palette))), diff_to_html(x)) |>
    html_print()
}
