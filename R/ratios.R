#' Harmonious ratios
#'
#' A simple named vector of "harmonious" ratios.
#'
#' Ratios fall into the following categories:
#'
#'  - [chromatic](https://en.wikipedia.org/wiki/Chromatic_scale) ratios derived
#'    from western music theory
#'  - [golden ratio](https://en.wikipedia.org/wiki/Golden_ratio)
#'  - "triple" and "double octave" ratios -- so set includes simple ratios of
#'    1:1 (unison), 1:2 (octave), 1:3, and 1:4
#'
#' All ratios (except for 1:1) appear in pairs, e.g. "major third" (1.25) and
#' "inverse major third" (0.8). Ratios greater than one imply a *portrait*
#' orientation, and ratios less than one imply a *landscape* orientation. Vector
#' is sorted from square (unison) to the tallest/widest proportions.
#'
#' @format A named vector
#'
#' @seealso `vignette("ratios")`
#'
#' @examples cbind(ratios)
"ratios"
