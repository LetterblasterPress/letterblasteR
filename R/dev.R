#' Find paths to package files (internal function)
#'
#' R packages may include arbitrary files such as drivers and templates that are
#' installed alongside the code itself. This function is a simple wrapper to
#' [base::system.file()] to find paths for this package.
#'
#' @param ... character vectors, specifying subdirectory and file(s) within this
#'   package. The default, none, returns the root of the package. Wildcards are
#'   not supported.
#'
#' @return Returns a character vector of file paths that matched `...` or an
#'   empty string if none matched.
#'
#' @examples
#' # Where is the custom dictionary for this installation?
#' letterblasteR:::inst("WORDLIST")
inst <- function(...) {
  system.file(..., package = "letterblasteR")
}
