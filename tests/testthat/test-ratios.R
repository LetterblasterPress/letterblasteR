test_that("degrees() converts legths to corresponding polar angles", {
  expect_identical(degrees(0, 1), 0)
  expect_identical(degrees(1, 1), 45)
  expect_identical(degrees(1, 0), 90)

  expect_identical(degrees(0, pi), 0)
  expect_identical(degrees(pi, pi), 45)
  expect_identical(degrees(pi, 0), 90)
})
