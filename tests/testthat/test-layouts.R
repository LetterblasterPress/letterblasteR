test_that("plot_layouts() plots all layouts or a subset", {
  plot_all <- plot_layouts(layouts)

  plot_some <- layouts |>
    subset(
      layouts$page_ratio %in% c("fourth", "fifth") &
        dplyr::between(ratios[layouts$text_ratio], 1.2, 1.6) &
        dplyr::between(layouts$text_pct, 0.5, 0.75)
    ) |>
    plot_layouts()

  expect_snapshot(ggplot2::layer_data(plot_all))
  expect_snapshot(ggplot2::layer_data(plot_some))
})

test_that("fit_layouts_to_text_width() does some sensible math", {
  y <- fit_layouts_to_text_width(4)
  expect_identical(
    names(y),
    c(names(layouts), "font_size", "min_lines", "max_lines")
  )
  expect_true(all(y$page_height < 8.5))
  expect_true(all(y$page_width < 5.5))
  expect_true(max(abs(diff(y$text_width, 4))) < 1e-12)
  expect_true(all(y$font_size == 10))
})

test_that("fit_layouts_to_text_width() estimates lines according to text size", {
  y <- fit_layouts_to_text_width(4)
  y12 <- fit_layouts_to_text_width(4, fontsize = "12pt")

  expect_true(all(y$font_size == 10))
  expect_true(all(y12$font_size == 12))
  expect_true(mean(y$min_lines) > mean(y12$min_lines))
  expect_true(mean(y$max_lines) > mean(y12$max_lines))
})
