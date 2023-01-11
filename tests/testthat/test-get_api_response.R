context("test-get_api_response")

test_that("get_api_response() function works!", {
  url <- "https://api.spacexdata.com/v3/capsules"
  handle <- Credentials$new("", "")
  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  response <- get_api_response(url, profile = handle)
  expect_s3_class(response, "response")
})
