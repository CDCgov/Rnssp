context("test-remove_rmd_template")

test_that("remove_rmd_template() works!", {
  add_rmd_template("text_mining", restart = FALSE)
  expect_message(remove_rmd_template("text_mining", restart = FALSE))
  expect_error(remove_rmd_template("text_mining", restart = FALSE))
})
