context("test-get_essence_data")

test_that("get_essence_data() function works!", {
  url <- "http://httpbin.org/json"
  handle <- Credentials$new("", "")
  expect_error(get_essence_data(url, start_date = "2021-02-15",
                                end_date = "2021-02-15", profile = handle))
  expect_error(get_essence_data(url, profile = handle))
})
