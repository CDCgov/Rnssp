context("test-get_api_response_method")

test_that("$get_api_response() method works!", {
  url <- "http://httpbin.org/json"
  handle <- Credentials$new("", "")
  handle2 <- Credentials$new()
  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  response <- handle$get_api_response(url)
  expect_s3_class(response, "response")
  expect_message(handle2$get_api_response(url))
})
