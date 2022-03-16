context("test-remove_rmd_template")

test_that("remove_rmd_template() works!", {
  # expect_error(remove_rmd_template(""))
  expect_error(remove_rmd_template("text_mining"))
})
