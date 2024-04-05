devtools::load_all()

# CLOBBER
fs::dir_ls(inst("mocks")) |>
  purrr::discard(~ fs::path_ext(.) %in% c("qmd", "R")) |>
  unlink(recursive = TRUE)

Sys.sleep(5) # allow any background git tasks to recognize deleted files

# render
quarto::quarto_render(inst("mocks/mock.qmd"), "all")

# create tidied versions
inst("mocks/mock.txt") |>
  to_tidy_md("markdown") |>
  as.character() |>
  write(path(inst("mocks"), "tidy_mock-noimages.md"))

with_dir(
  inst("mocks"),
  "mock.html" |>
    to_tidy_md("html") |>
    as.character() |>
    write("tidy_mock.html.md")
)

with_dir(
  inst("mocks"),
  "mock.epub" |>
    to_tidy_md("epub") |>
    as.character() |>
    write("tidy_mock.epub.md")
)
