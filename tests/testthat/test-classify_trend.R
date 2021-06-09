context("test-classify_trend")

test_that("classify_trend functions as expected!", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    dataCount = floor(runif(366, min = 0, max = 101)),
    allCount = floor(runif(366, min = 101, max = 500))
  )
  df_trend <- classify_trend(df)

  expect(is.data.frame(df_trend), failure_message = "classify_trend test fails!")
  expect_equal(length(df_trend), length(df) + 6)
})
