context("test-create_apikey_profile")

test_that("create_apikey_profile() works!", {
  myProfile <- create_apikey_profile("abc1234567890")
  testthat::expect_type(myProfile, "environment")
  testthat::expect_s3_class(myProfile, "R6")
  testthat::expect_s3_class(myProfile, "NSSPApikey")
})
