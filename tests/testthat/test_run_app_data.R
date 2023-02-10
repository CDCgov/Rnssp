context("test-run_app")

test_that("run_app() function works!", {
  expect_error(run_app("noapp"))
})
