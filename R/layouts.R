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

#' Visualize harmonious layouts
#'
#' This function plots harmonious layouts, using transparency to overlay
#' multiple similar layouts at a time. It is meant to give an overview of
#' layout options and to home in on a subset of candidates for a project.
#'
#' @param x subset of `layouts`
#'
#' @return Returns a ggplot.
#' @export
#'
#' @examples
#' plot_layouts(layouts)
#'
#' layouts |>
#'   subset(
#'     layouts$page_ratio %in% c("fourth", "fifth") &
#'       dplyr::between(ratios[layouts$text_ratio], 1.2, 1.6) &
#'       dplyr::between(layouts$text_pct, 0.5, 0.75)
#'   ) |>
#'   plot_layouts()
plot_layouts <- function(x) {
  x |>
    transmute(
      across(ends_with("ratio")),
      text_pct,
      page_left__x1 = -page_width,
      page_left__x2 = 0,
      page_left__y1 = -0.5 * page_height,
      page_left__y2 = 0.5 * page_height,
      page_right__x1 = 0,
      page_right__x2 = page_width,
      page_right__y1 = -0.5 * page_height,
      page_right__y2 = 0.5 * page_height,
      text_left__x1 = -i_mar - text_width,
      text_left__x2 = -i_mar,
      text_left__y1 = -0.5 * page_height + b_mar,
      text_left__y2 = -0.5 * page_height + b_mar + text_height,
      text_right__x1 = i_mar,
      text_right__x2 = i_mar + text_width,
      text_right__y1 = -0.5 * page_height + b_mar,
      text_right__y2 = -0.5 * page_height + b_mar + text_height
    ) |>
    pivot_longer(matches("__")) |>
    separate("name", into = c("box", "name"), sep = "__") |>
    pivot_wider() |>
    mutate(alpha = if_else(grepl("page", .data$box), 1, 0.5)) |>
    arrange(page_ratio, text_ratio, .data$box, desc(text_pct)) |>
    ggplot(aes(xmin = .data$x1, xmax = .data$x2, ymin = .data$y1, ymax = .data$y2)) +
    geom_rect(aes(fill = .data$box, alpha = .data$alpha), colour = NA) +
    scale_fill_manual(values = c("white", "white", "black", "black")) +
    facet_grid(
      fct_rev(fct_relabel(page_ratio, ~ sub(" ", "\n", .))) ~
        fct_relabel(text_ratio, ~ sub(" ", "\n", .)),
      switch = "y"
    ) +
    theme(
      aspect.ratio = 1,
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      panel.background = element_rect(fill = "grey35"),
      strip.background = element_rect(fill = "white"),
      panel.spacing = unit(-1, "point"),
    ) +
    labs(caption = sprintf("%s layouts", format(nrow(x), big.mark = ",")))
}

#' Scale generic layouts
#'
#' Resize generic layouts to fit a desired textblock width, and filter out
#' layouts that are too large for the specified stock paper.
#'
#' @param target_text_width desired textblock width in inches
#' @param fontsize base font size for document, used to estimate the number of
#'   lines of text the textblock can accommodate
#' @param max_page_height,max_page_width maximum page dimensions (default
#'   assumes layout is printed on letter paper folded in half)
#'
#' @return Returns [layouts] plus three additional columns: `font_size`,
#'   `min_lines`, & `max_lines`.
#' @export
#'
#' @examples
#' str(fit_layouts_to_text_width(4))
fit_layouts_to_text_width <- function(
    target_text_width,
    fontsize = c("10pt", "11pt", "12pt"),
    max_page_height = 8.5,
    max_page_width = 5.5) {
  font_size <- as.integer(gsub("^(\\d+)pt$", "\\1", match.arg(fontsize)))

  layouts |>
    mutate(
      factor = target_text_width / text_width,
      page_height = .data$factor * .data$page_height,
      page_width = .data$factor * .data$page_width,
      text_height = .data$factor * .data$text_height,
      text_width = .data$factor * .data$text_width,
      b_mar = .data$factor * .data$b_mar,
      t_mar = .data$factor * .data$t_mar,
      o_mar = .data$factor * .data$o_mar,
      i_mar = .data$factor * .data$i_mar
    ) |>
    select(-any_of("factor")) |>
    filter(
      .data$page_height < max_page_height,
      .data$page_width < max_page_width
    ) |>
    mutate(
      font_size,
      min_lines = ceiling(.data$text_height / (1.2 * font_size / 72.27)),
      max_lines = floor(.data$text_height / (font_size / 72.27))
    )
}
