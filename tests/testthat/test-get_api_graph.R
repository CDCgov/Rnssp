context("test-get_api_graph")

test_that("get_api_graph() function works!", {
  url <- "http://httpbin.org/image/png"
  handle <- Credentials$new("", "")
  graph <- get_api_graph(url, profile = handle)
  expect_s3_class(graph$api_response, "response")
})
