context("test-get_api_response")

test_that("get_api_response() function works!", {
  url <- "http://httpbin.org/json"
  handle <- Credentials$new("", "")
  handle2 <- Token$new("abc1234567890")
  handle3 <- Apikey$new("")

  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")

  expect_s3_class(handle2, "R6")
  expect_s3_class(handle2, "NSSPToken")

  expect_s3_class(handle3, "R6")
  expect_s3_class(handle3, "NSSPApikey")

  response <- get_api_response(url, profile = handle)
  response2 <- get_api_response(url, profile = handle2)
  response3 <- get_api_response(url, profile = handle3)

  expect_s3_class(response, "response")
  expect_s3_class(response2, "response")
  expect_s3_class(response3, "response")
})
