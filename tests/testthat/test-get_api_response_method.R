context("test-get_api_response_method")

test_that("$get_api_response() method works!", {
  url <- "http://httpbin.org/json"

  handle <- Credentials$new("", "")
  handle2 <- Credentials$new()

  handle3 <- Token$new("abc1234567890")
  handle4 <- Token$new()

  handle5 <- Apikey$new("abc1234567890")
  handle6 <- Apikey$new()

  container <- NSSPContainer$new()

  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPAuth")
  expect_s3_class(handle, "NSSPCredentials")
  expect_s3_class(handle3, "R6")
  expect_s3_class(handle3, "NSSPAuth")
  expect_s3_class(handle3, "NSSPToken")
  expect_s3_class(container, "NSSPContainer")

  response <- handle$get_api_response(url)
  response2 <- handle3$get_api_response(url)
  response3 <- handle4$get_api_response(url)

  expect_s3_class(response, "response")
  expect_s3_class(response2, "response")
  expect_s3_class(response3, "response")
  expect_message(handle2$get_api_response(url))
  expect_message(handle4$get_api_response(url))
  expect_message(handle6$get_api_response(url))
})
