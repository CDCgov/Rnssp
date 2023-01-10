context("test-get_api_data")

test_that("get_api_data() function works!", {
  url <- "http://httpbin.org/json"
  url2 <- "http://httpbin.org/robots.txt"
  handle <- Credentials$new("", "")
  expect_s3_class(handle, "R6")
  expect_s3_class(handle, "NSSPCredentials")
  data <- get_api_data(url, profile = handle)
  data2 <- get_api_data(url2, profile = handle, fromCSV = TRUE)
  expect_s3_class(data$slideshow$slides, "data.frame")
  expect_s3_class(data2, "data.frame")
})
