context("test-alert_regression")

test_that("alert_regression functions as expected!", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    count = floor(runif(366, min = 0, max = 101))
  )
  df_regression <- alert_regression(df, t = date, y = count)

  expect(is.data.frame(df_regression), failure_message = "alert_regression test fails!")
  expect_equal(length(df_regression), length(df) + 5)
})
