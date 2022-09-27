context("test-create_profile")

test_that("create_profile() works!", {
  myProfile <- create_profile("", "")
  testthat::expect_type(myProfile, "environment")
  testthat::expect_s3_class(myProfile, "R6")
  testthat::expect_s3_class(myProfile, "NSSPCredentials")
})
