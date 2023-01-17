context("test-get_api_data_method")

test_that("$get_api_data() method works!", {
  url <- "http://httpbin.org/json"
  handle <- Credentials$new("", "")
  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  data <- handle$get_api_data(url)
  expect_type(data, "list")
})
