context("test-explain_url")

test_that("explain_url correctly parses query parameters", {
  url <- "https://example.com/page?name=John&age=30&city=New%20York"
  result <- explain_url(url)

  expected <- data.frame(
    Key = c("name", "age", "city"),
    Value = c("John", "30", "New York"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("explain_url handles URLs without query parameters", {
  url <- "https://example.com/page"
  result <- explain_url(url)

  expect_true(nrow(result) == 0)
})

test_that("explain_url handles query parameters without values", {
  url <- "https://example.com/page?key1=&key2=value2"
  result <- explain_url(url)

  expected <- data.frame(
    Key = c("key1", "key2"),
    Value = c("", "value2"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("explain_url handles URLs with multiple '=' signs in values", {
  url <- "https://example.com/page?query=this=is=a=test"
  result <- explain_url(url)

  expected <- data.frame(
    Key = "query",
    Value = "this",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})

test_that("explain_url decodes URL-encoded characters", {
  url <- "https://example.com/page?message=Hello%20World%21"
  result <- explain_url(url)

  expected <- data.frame(
    Key = "message",
    Value = "Hello World!",
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})
