context("test-create_token_profile")

test_that("create_token_profile() works!", {
  myProfile <- create_token_profile("abc1234567890")
  testthat::expect_type(myProfile, "environment")
  testthat::expect_s3_class(myProfile, "R6")
  testthat::expect_s3_class(myProfile, "NSSPToken")
})
