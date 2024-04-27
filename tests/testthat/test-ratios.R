## degrees() ###################################################################

test_that("degrees() converts legths to corresponding polar angles", {
  expect_identical(degrees(0, 1), 0)
  expect_identical(degrees(1, 1), 45)
  expect_identical(degrees(1, 0), 90)

  expect_identical(degrees(0, pi), 0)
  expect_identical(degrees(pi, pi), 45)
  expect_identical(degrees(pi, 0), 90)
})

## round_ratios() ##############################################################

test_that("round_ratios() identifies exact ratios", {
  expect_identical(round_ratios(ratios), ratios)
})

test_that("round_ratios() identifies similar ratios", {
  expect_identical(round_ratios(ratios + 1e-6), ratios)
  expect_identical(round_ratios(ratios - 1e-6), ratios)
})

test_that("round_ratios() fails to identify similar ratios outside tolerance", {
  na_ratios <- ratios[rep(NA, length(ratios))]

  expect_identical(round_ratios(ratios + 1e-6, tolerance = 1e-8), na_ratios)
  expect_identical(round_ratios(ratios - 1e-6, tolerance = 1e-8), na_ratios)
})

test_that('round_ratios() identifies high ratios when target = "taller", otherwise NA', {
  na_ratios <- ratios[rep(NA, length(ratios))]

  expect_identical(round_ratios(ratios + 1e-6, target = "taller"), ratios)
  expect_identical(round_ratios(ratios - 1e-6, target = "taller"), na_ratios)
})

test_that('round_ratios() identifies low ratios when target = "wider", otherwise NA', {
  na_ratios <- ratios[rep(NA, length(ratios))]

  expect_identical(round_ratios(ratios + 1e-6, target = "wider"), na_ratios)
  expect_identical(round_ratios(ratios - 1e-6, target = "wider"), ratios)
})

test_that("round_ratios() accpets changes to the width argument", {
  expect_identical(round_ratios(3, 2), c("fifth" = 1.5))
  expect_identical(round_ratios(2, 3), c("inverse fifth" = 1 / 1.5))
  expect_identical(
    round_ratios(3:2, 2:3),
    c("fifth" = 1.5, "inverse fifth" = 1 / 1.5)
  )
})

## name_ratios() ###############################################################

test_that("name_ratios() returns a character vector of (possibly NA) names", {
  expect_identical(name_ratios(3, 2), "fifth")
  expect_identical(name_ratios(3 / 2), "fifth")
  expect_identical(name_ratios(pi), as.character(NA))
  expect_identical(name_ratios(ratios), names(ratios))
})
