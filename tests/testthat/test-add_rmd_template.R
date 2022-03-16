context("test-add_rmd_template")

test_that("add_rmd_template() works!", {
  expect_error(add_rmd_template(""))
  # expect_known_output(add_rmd_template("text_mining"), tempfile(), print = TRUE)
})
