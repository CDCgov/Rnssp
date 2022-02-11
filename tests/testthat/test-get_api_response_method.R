context("test-get_api_response_method")

test_that("$get_api_response() method works!", {
  url <- "https://api.spacexdata.com/v3/capsules"
  handle <- Credentials$new("", "")
  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  response <- handle$get_api_response(url)
  expect_s3_class(response, "response")
})
