test_that("plot_layouts() plots all layouts or a subset", {
  plot_all <- plot_layouts(layouts)

  plot_some <- layouts |>
    subset(
      layouts$page_ratio %in% c("fourth", "fifth") &
        dplyr::between(ratios[layouts$text_ratio], 1.2, 1.6) &
        dplyr::between(layouts$text_pct, 0.5, 0.75)
    ) |>
    plot_layouts()

  vdiffr::expect_doppelganger("Plot all layouts", plot_all)
  vdiffr::expect_doppelganger("Plot subset of layouts", plot_some)
})
