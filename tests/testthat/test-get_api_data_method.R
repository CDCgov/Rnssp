context("test-get_api_data_method")

test_that("$get_api_data() method works!", {
  url <- "http://httpbin.org/json"
  url2 <- "http://httpbin.org/robots.txt"

  handle <- Credentials$new("", "")
  handle2 <- Token$new("")

  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  expect_s3_class(handle2, "R6")
  expect_s3_class(handle2, "NSSPToken")

  data <- handle$get_api_data(url)
  data2 <- handle$get_api_data(url2, fromCSV = TRUE)

  data3 <- handle2$get_api_data(url)
  data4 <- handle2$get_api_data(url2, fromCSV = TRUE)

  expect_type(data, "list")
  expect_s3_class(data2, "data.frame")
  expect_type(data3, "list")
  expect_s3_class(data4, "data.frame")
})
