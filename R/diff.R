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
  tmp_dir <- dir_create(path_temp("git"))
  tmp_pth <- path(tmp_dir, "file")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  git_init(tmp_dir)

  write(a, tmp_pth)
  git_add("file", repo = tmp_pth)
  git_commit("a", repo = tmp_pth)
  write(b, tmp_pth)

  with_dir(
    tmp_dir,
    system(
      "git diff -U0 --minimal --color-words",
      intern = TRUE
    )
  )
}
