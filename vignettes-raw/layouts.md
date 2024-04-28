---
title: "Harmonious layouts"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Harmonious layouts}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
keep-md: true
---





# Background

-   Elements of Typographic Style
-   String harmonics

# Objective

Identify a set of generic (dimensionless) page layouts suitable for codex-bound
text.

# Approach

Of course, there are infinite ways a rectangular textblock could be placed on a
rectangular page. Here, we will restrict the most important page layout
proportions to compatible combinations of harmonious ratios (see
`vignette("ratios")`). Then, we'll run a grid search for each combination,
considering a range of relative textblock sizes. Margins and secondary page
layout proportions are calculated, and `round_ratios()` is used to identify
solutions that serendipitously produce harmonious secondary proportions.

## Exact proportions (combinatoric)

-   page (square or portrait)
-   two-page spread
-   textblock (portrait, different than page proportions)
-   bottom–top margins (bottom margin larger than top margin)
-   outer–top margins (outer margin equal to or larger than top margin)
-   outer–bottom margins


::: {.cell}

```{.r .cell-code}
combos <- expand_grid(
  page_ratio = ratios[ratios >= 1 & (2 / ratios) %in% ratios],
  text_ratio = ratios[ratios > 1],
  bt_ratio = ratios[ratios > 1],
  ot_ratio = ratios[ratios >= 1],
  ob_ratio = ratios
) |>
  filter(page_ratio != text_ratio, ot_ratio == ob_ratio * bt_ratio)
```
:::


## Approximate proportions (grid search)

Next, we'll consider secondary proportions that include the inner margin. It is
overly restrictive (if not impossible) to expect every proportion to fall into
perfect harmony, so we want to consider nearly perfect solutions. Given the
optical foreshortening created by pages that curve towards the center spine,
deviations that produce a slightly wider proportion may even work in our favor.

Secondary inner-margin proportions:

-   textblock, two-up (spanning inner margins)
-   outer–inner margins
-   top–inner margins
-   bottom–inner margins
-   outer–inner (2⨉) margins, two-up
-   top–inner (2⨉) margins, two-up
-   bottom–inner (2⨉) margins, two-up

For clarity and ease of development, we'll wrap our grid-search logic in a
[memoised](https://en.wikipedia.org/wiki/Memoization) function.


::: {.cell}

```{.r .cell-code}
glissando <- memoise::memoise(
  cache = cachem::cache_disk(fs::dir_create("memoise_cache")),
  function(x, pct_height_min = 0.10, pct_height_max = 0.98, n = 1e5) {
    message(sprintf(
      "%s -- %s",
      name_ratios(unique(x$page_ratio)),
      name_ratios(unique(x$text_ratio))
    ))

    x |>
      # expand dataset to include a range of textblock heights
      mutate(
        across(everything(), as.numeric), ## reduce object size by removing names
        page_height = page_ratio,
        page_width = 1,
        join = 1L
      ) |>
      left_join(by = "join", relationship = "many-to-many", tibble(
        join = 1L,
        text_height = seq(
          pct_height_min * unique(x$page_ratio),
          pct_height_max * unique(x$page_ratio),
          length.out = n
        )
      )) |>
      select(-join) |>
      # calculate margins for each layout
      mutate(
        text_width = text_height / text_ratio,
        b_mar = (bt_ratio * (page_height - text_height)) / (bt_ratio + 1),
        t_mar = (page_height - text_height) / (bt_ratio + 1),
        o_mar = t_mar * ot_ratio,
        i_mar = page_width - o_mar - text_width,
        oi_ratio = round_ratios(o_mar, i_mar, "wider")
      ) |>
      filter(i_mar > 0, !is.na(oi_ratio)) |>
      # calculate secondary ratios and tidy tibble
      transmute(
        page_ratio, # exact
        page_2up_ratio = page_height / (2 * page_width), # exact
        text_ratio, # exact
        text_2up_ratio = round_ratios(text_height, 2 * (text_width + i_mar), "wider"),
        bt_ratio, # exact
        oi_ratio, # rounded

        ot_ratio, # exact
        ob_ratio, # exact
        ti_ratio = round_ratios(t_mar, i_mar, "wider"),
        bi_ratio = round_ratios(b_mar, i_mar, "wider"),
        oii_ratio = round_ratios(o_mar, 2 * i_mar, "wider"),
        tii_ratio = round_ratios(t_mar, 2 * i_mar, "wider"),
        bii_ratio = round_ratios(b_mar, 2 * i_mar, "wider"),
        text_pct = (text_height * text_width) / (page_height * page_width),
        page_height, page_width,
        text_height, text_width,
        b_mar, t_mar, o_mar, i_mar
      ) |>
      drop_na() |>
      mutate(across(
        matches("^(page|page_2up|text|bt|ot|ob)_ratio"),
        ~ ratios[name_ratios(.)]
      ))
  }
)
```
:::


With our `glissando()` function defined, we can identify harmonious ratios for
each of our primary layout combinations.


::: {.cell}

```{.r .cell-code}
layouts <- combos |>
  split(with(combos, list(names(page_ratio), names(text_ratio))), drop = TRUE) |>
  map_dfr(glissando)
```
:::


These layouts have quite a bit of redundancy with multiple nearly identical
layouts that fall within our tolerances. Let's do one more round, focusing on
these solution zones, then we can choose a single best layout for each motif.
Clever trigonometry could perhaps identify some local maxima, but the resolution
of our grid search already takes us well beyond the resolution of commercial
printers.


::: {.cell}

```{.r .cell-code}
layouts <- layouts |>
  bind_rows(
    layouts |>
      group_by(across(ends_with("ratio"))) |>
      summarise(
        pct_height_min = 0.9999 * min(text_height / page_height),
        pct_height_max = 1.0001 * max(text_height / page_height),
        .groups = "drop"
      ) |>
      nest(x = names(combos)) |>
      transmute(x, pct_height_min, pct_height_max) |>
      pmap_dfr(glissando)
  ) |>
  distinct()
```
:::


Finally, let's eliminate redundancy by favoring the set of dimensions that
result in the smallest total dissonance (as measured by area beyond the confines
of an ideally proportioned rectangle).


::: {.cell}

```{.r .cell-code}
layouts <- layouts |>
  mutate(
    text_2up_err = (text_height * 2 * (text_width + i_mar)) -
      (text_height^2 / text_2up_ratio),
    oi_err = text_height * (i_mar - o_mar / oi_ratio),
    ti_err = (t_mar * i_mar) - (t_mar^2 / ti_ratio),
    bi_err = (b_mar * i_mar) - (b_mar^2 / bi_ratio),
    oii_err = text_height * (2 * i_mar - o_mar / oii_ratio),
    tii_err = (t_mar * 2 * i_mar) - (t_mar^2 / tii_ratio),
    bii_err = (b_mar * 2 * i_mar) - (b_mar^2 / bii_ratio),
    error = as.numeric(
      abs(text_2up_err) +
        2 * oi_err + 2 * ti_err + 2 * bi_err +
        oii_err + tii_err + bii_err
    )
  )

# stopifnot(all(layouts$text_2up_err >= 0))
stopifnot(all(layouts$oi_err >= 0))
stopifnot(all(layouts$ti_err >= 0))
stopifnot(all(layouts$bi_err >= 0))
stopifnot(all(layouts$oii_err >= 0))
stopifnot(all(layouts$tii_err >= 0))
stopifnot(all(layouts$bii_err >= 0))

layouts |>
  group_by(across(ends_with("ratio"))) |>
  summarise(range = diff(range(text_height)), .groups = "drop") |>
  with(range < 0.001) |>
  all() |>
  stopifnot()

layouts <- layouts |>
  slice_min(error, by = ends_with("ratio")) |>
  select(-ends_with("_err")) |>
  mutate(across(
    ends_with("ratio"),
    ~ droplevels(factor(names(.), levels = names(ratios)))
  ))
```
:::


