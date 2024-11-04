context("test-askme")

test_that("askme() works!", {
  options(askme = function(...) {
    "password"
  })
  testthat::expect_silent(askme())
})
