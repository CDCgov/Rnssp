context("test-get_api_tsgraph")

test_that("get_api_tsgraph() function works!", {
  url <- "http://httpbin.org/image/png"

  handle <- Credentials$new("", "")
  handle2 <- Token$new("abc1234567890")

  tsgraph <- get_api_tsgraph(url, profile = handle)
  tsgraph2 <- handle$get_api_tsgraph(url)

  expect_s3_class(tsgraph$api_response, "response")
  expect_s3_class(tsgraph2$api_response, "response")

  expect_error(get_api_tsgraph(url, profile = handle2))
})
