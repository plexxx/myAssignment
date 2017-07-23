#' Super minimal test suite
#'
#' Tests that the function \code{make_filename} returns the expected string for
#' specific input value, namely for \code{2014}.
#' @importFrom testthat test_that
#' @importFrom testthat expect_that
#' @importFrom testthat equals
#' @importFrom myAssignment make_filename
testthat::test_that("myTest", {
  filename <- myAssignment::make_filename(2014)
  testthat::expect_that(filename, testthat::equals("accident_2014.csv.bz2"))
})
