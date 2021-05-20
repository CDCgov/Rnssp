context("test-alert_ewma")

test_that("alert_ewma functions as expected!", {
  df <- data.frame(date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
                   count = floor(runif(366, min=0, max=101)))
  df_ewma <- alert_ewma(df)

  expect(is.data.frame(df_ewma),failure_message = "alert_ewma test fails!")
  expect_equal(length(df_ewma), length(df) + 8)
})
