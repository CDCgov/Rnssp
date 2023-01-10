context("test-get_api_tsgraph")

test_that("get_api_graph() function works!", {
  url <- "http://httpbin.org/image/png"
  handle <- Credentials$new("", "")
  tsgraph <- get_api_tsgraph(url, profile = handle)
  expect_s3_class(tsgraph$api_response, "response")
})
