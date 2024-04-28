#' Harmonious layouts
#'
#' Generic page layout options suitable for bound codices. Each page layout
#' attempts to shape the page, textblock, and whitespace between them using
#' only harmonious ratios.
#'
#' All layouts are sized so the page width equals one. This allows the actual
#' size to be scaled to suit the desired font and line spacing.
#'
#' It is seldom possible to lock all thirteen ratios into perfect harmony, so
#' by design inner margins are allowed to be slightly wider than ideal if
#' needed. The curvature of pages towards the spine produces some optical
#' foreshortening, so this approach works to counteract this effect. Fit is
#' assessed by summing all inner margin whitespace in excess of the ideal ratio.
#' This `error` is typically very small and is often smaller than the resolution
#' of commercial printers.
#'
#' @format A [tibble][tibble::tibble-package] of generic harmonious layouts
#' \describe{
#'   \item{page_ratio}{page proportion, height:width}
#'   \item{page_2up_ratio}{two-page proportion, height:(2 × width)}
#'   \item{text_ratio}{textblock proportion, height:width}
#'   \item{text_2up_ratio}{two-page textblock proportion, height:(2 × width +
#'     2 × inner margin)}
#'   \item{bt_ratio,oi_ratio,ot_ratio,ob_ratio,ti_ratio,bi_ratio}{bottom (b),
#'     top (t), outer (o), and inner (i) margin ratios}
#'   \item{oii_ratio,tii_ratio,bii_ratio}{two-page margin ratios that span the
#'     spine (i.e., width = 2 × inner margin)}
#'   \item{text_pct}{textblock area as a fraction of the total page area}
#'   \item{page_height,page_width}{page dimensions, relative to unit width of 1}
#'   \item{text_height,text_width}{textblock dimensions, relative to unit width
#'     of 1}
#'   \item{b_mar,t_mar,o_mar,i_mar}{margin dimensions, relative to unit width of
#'     1}
#'   \item{error}{total area of layout that is in disharmony, see Details}
#' }
#'
#' @seealso `vignette("layouts")`
#' @seealso `vignette("ratios")`
#'
#' @examples
#' str(layouts)
"layouts"
