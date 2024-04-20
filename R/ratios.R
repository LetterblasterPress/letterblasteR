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

#' Convert proportions to angles
#'
#' This trigonometric helper calculates the angle of the diagonal for a
#' rectangle of given dimensions.
#'
#' @param h,w pairwise height & width vectors representing input proportions
#'
#' @return Returns a vector of angles in degrees
#'
#' @export
#'
#' @examples
#' degrees(0, 1)
#' degrees(1, 1)
#' degrees(1, 0)
degrees <- function(h, w = 1) {
  180 * atan2(h, w) / pi
}

#' Round proportions to nearest harmonious ratios
#'
#' Given height & width vectors representing input proportions, this function
#' identifies the nearest harmonious ratio.
#'
#' Input proportions are compared to harmonious ratios after converting to
#' angles with [degrees()]. Rounding only considers harmonious ratios that are
#' within ±tolerance (in degrees) of the input proportion.
#'
#' By default, the nearest harmonious ratio is returned, but the `target`
#' argument can be used to favor actual proportions that run tall or wide. When
#' making page layout decisions, it can be useful to prefer proportions along
#' the spine that are a little wider than the ideal ratio. Once bound, optical
#' foreshortening along the spine tends to bring the perceived ratio back into
#' harmony.
#'
#' @param h,w pairwise height & width vectors representing input proportions
#' @param target whether to identify nearest harmonious ratio (default) or to
#'   limit to solutions where the input proportion may only be taller/wider
#'   than the harmonious ratio, see Details & Examples
#' @param tolerance only consider harmonious ratios within ±tolerance of input
#'   ratios, in degrees
#'
#' @return Returns a named vector of rounded proportions. Proportions that are
#' not within ±tolerance of a harmonious ratio with be `NA`.
#'
#' @export
#'
#' @seealso `vignette("ratios")`
#'
#' @examples
#' round_ratios(3, 2)
#'
#' round_ratios(2.9999, 2)
#' round_ratios(3.0001, 2)
#'
#' round_ratios(c(2.9999, 3.0001), 2, tolerance = 1e-6)
#'
#' round_ratios(c(2.9999, 3.0001), 2, target = "taller")
#' round_ratios(c(2.9999, 3.0001), 2, target = "wider")
round_ratios <- function(
    h, w = 1, target = c("nearest", "taller", "wider"), tolerance = 0.01) {
  target <- match.arg(target)
  a <- degrees(h, w)

  index <- degrees(ratios) |>
    map_dfc(function(target_a) {
      delta <- a - target_a
      delta[abs(delta) > tolerance] <- NA
      if (target == "taller") delta[delta < 0] <- NA
      if (target == "wider") delta[delta > 0] <- NA
      return(delta)
    }) |>
    apply(1, function(x) c(which.min(x), NA)[1])

  ratios[index]
}
