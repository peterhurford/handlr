context("errors")

test_that("it works if it is not an error", {
  expect_equal(2, with_errors_as("error!", { 1 + 1 }))
  expect_equal(100, errors_are_null(100))
})

test_that("errors are something else", {
  expect_equal(with_errors_as("error!", stop("lol")), "error!")
  expect_equal(errors_are_na(stop("lol")), NA)
  expect_equal(errors_are_null(stop("lol")), NULL)
  expect_equal(errors_are_false(stop("lol")), FALSE)
})

test_that("errors are warnings", {
  expect_warning(errors_are_warnings(stop("lol")), "lol")
})
