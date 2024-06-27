context("test-get_api_graph")

test_that("get_api_graph() function works!", {
  url <- "http://httpbin.org/image/png"

  handle <- Credentials$new("", "")
  handle2 <- Token$new("abc1234567890")
  handle3 <- Apikey$new("abc1234567890")

  graph <- get_api_graph(url, profile = handle)
  graph2 <- handle$get_api_graph(url)
  graph3 <- get_api_graph(url, profile = handle2)
  graph4 <- handle2$get_api_graph(url)
  graph5 <- get_api_graph(url, profile = handle3)
  graph6 <- handle3$get_api_graph(url)

  expect_s3_class(graph$api_response, "response")
  expect_s3_class(graph2$api_response, "response")
  expect_s3_class(graph3$api_response, "response")
  expect_s3_class(graph4$api_response, "response")
  expect_s3_class(graph5$api_response, "response")
  expect_s3_class(graph6$api_response, "response")

  expect_error(expect_message(get_api_graph(url, profile = list())))
})
