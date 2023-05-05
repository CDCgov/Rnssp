context("test-get_api_data")

test_that("get_api_data() function works!", {
  url <- "https://httpbin.org/json"
  url2 <- "http://httpbin.org/robots.txt"

  handle <- Credentials$new("", "")
  handle2 <- Token$new("abc1234567890")

  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  expect_s3_class(handle2, "R6")
  expect_s3_class(handle2, "NSSPToken")

  data <- get_api_data(url, profile = handle)
  data2 <- get_api_data(url2, profile = handle, fromCSV = TRUE)

  data3 <- get_api_data(url, profile = handle2)
  data4 <- get_api_data(url2, profile = handle2, fromCSV = TRUE)

  expect_type(data, "list")
  expect_s3_class(data2, "data.frame")
  expect_type(data3, "list")
  expect_s3_class(data4, "data.frame")
})
