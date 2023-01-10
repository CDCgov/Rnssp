context("test-add_rmd_template")

test_that("add_rmd_template() works!", {
  expect_error(add_rmd_template(""))
  expect_error(add_rmd_template("NEWS.md"))
  expect_message(add_rmd_template("state_ed_report", restart = FALSE))
})
