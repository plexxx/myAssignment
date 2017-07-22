library(testthat)

#' Super minimal test suite
#'
#' Tests that the function \code{make_filename} returns the expected string for
#' specific input value, namely for \code{2014}.
#' @importFrom testthat test_that
#' @importFrom testthat expect_that
#' @importFrom testthat equals
test_that("myTest", {
  filename <- make_filename(2014)
  expect_that(filename, equals("accident_2014.csv.bz2"))
})
