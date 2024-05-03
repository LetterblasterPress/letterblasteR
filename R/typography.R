#' Generate sample text
#'
#' Generate a sample of "Harvard sentences", wrapping to an exact column width
#' if desired.
#'
#' @param n number of sentences to return, or number of lines if `wrap` is not
#'   `NULL`
#' @param wrap column to wrap text at, optional
#'
#' If `wrap` is specified, text is wrapped exactly at that column, disregarding
#' language. Lines are rearranged at random after filtering out lines with
#' leading or trailing space.
#'
#' @return
#' @export
#'
#' @seealso [stringr::sentences]
#'
#' @examples
#' sample_text(3)
#' sample_text(3, wrap = 42)
sample_text <- function(n, wrap = NULL) {
  y <- stringr::sentences

  if (is.null(wrap)) {
    y <- sample(y, size = n, replace = n > length(y))
  } else {
    N <- n * wrap
    y <- sample(y, size = N, replace = (N) > length(y)) |>
      paste(collapse = " ") |>
      strsplit("") |>
      unlist()
    y <- split(y, ceiling(seq_along(y) / wrap)) |>
      map_chr(paste, collapse = "") |>
      keep(grepl, pattern = "^\\S.*\\S$") |>
      sample(n) |>
      as.character()
  }

  return(y)
}

#' Estimate text width (measure)
#'
#' This function typesets bits of text to estimate the width of a textblock
#' that averages `wrap` characters wide. This is useful for back-calculating
#' page dimensions to suit the optimal line length of 66 characters (or whatever
#' your project calls for).
#'
#' This rather brutish function uses [LuaLaTeX](https://www.latex-project.org)
#' with [fontspec](https://ctan.org/pkg/fontspec) and
#' [pdftools](https://docs.ropensci.org/pdftools/) for R, and it assumes that
#' your TeX installation is configured to access fonts by name. It makes no
#' attempt to check your system for required resources, handle errors, or
#' protect against injection of malicious code.
#'
#' @param font name of font, passed to `\setmainfont{}`
#' @param fontsize fontsize, passed to `\documentclass` options
#' @param wrap desired number of characters per line, passed to [sample_text()].
#'
#' @return Returns a named list with average ("mean") and standard deviation
#'   ("sd") of desired text width
#' @export
estimate_line_length <- function(
    font,
    fontsize = c("10pt", "11pt", "12pt"),
    wrap = 66) {
  template <- paste(collapse = "\n", c(
    "\\documentclass[%s]{article}",
    "\\usepackage{fontspec}",
    "\\defaultfontfeatures{Ligatures={Required, Common, Contextual, TeX}}",
    "\\setmainfont[Numbers={Proportional, OldStyle}]{%s}",
    "\\frenchspacing",
    "\\usepackage{calc}",
    "\\newlength{\\sw}",
    "\\begin{document}",
    "%s",
    "\\end{document}"
  ))

  tex <- template |>
    sprintf(
      match.arg(fontsize),
      font,
      sample_text(1e3, wrap = wrap) |>
        sprintf(fmt = "\\setlength{\\sw}{\\widthof{%s}}\\the\\sw") |>
        paste(collapse = "\n\n")
    )

  tmp_dir <- dir_create(file_temp("tex_"))
  on.exit(unlink(tmp_dir, recursive = TRUE))
  write(tex, path(tmp_dir, "measure.tex"))
  with_dir(tmp_dir, system("lualatex measure.tex", intern = TRUE))

  lengths <- path(tmp_dir, "measure.pdf") |>
    pdf_text() |>
    strsplit("\\s+") |>
    unlist() |>
    keep(grepl, pattern = "^\\d*\\.?\\d+pt$") |>
    sub(pattern = "pt", replacement = "") |>
    as.numeric()

  stopifnot(length(lengths) == 1e3)
  stopifnot(all(complete.cases(lengths)))

  lengths <- lengths / 72.27 # convert TeX points to inches
  c(mean = mean(lengths), sd = sd(lengths))
}
