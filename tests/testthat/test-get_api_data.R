context("test-get_api_data")

test_that("get_api_data() function works!", {
  url <- "https://api.spacexdata.com/v3/capsules"
  handle <- Credentials$new("", "")
  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  data <- get_api_data(url, profile = handle)
  expect_s3_class(data, "data.frame")
})
