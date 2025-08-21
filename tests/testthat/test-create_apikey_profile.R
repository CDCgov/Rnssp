context("test-create_apikey_profile")

test_that("create_apikey_profile() works!", {
  myProfile <- create_apikey_profile("abc1234567890")
  testthat::expect_type(myProfile, "environment")
  testthat::expect_s3_class(myProfile, "R6")
  testthat::expect_s3_class(myProfile, "NSSPApikey")

  myProfile2 <- create_apikey_profile("abc1234567890", key_name = "key")
  testthat::expect_type(myProfile2, "environment")
  testthat::expect_s3_class(myProfile2, "R6")
  testthat::expect_s3_class(myProfile2, "NSSPApikey")
})
