context("test-alert_nbinom")

test_that("alert_nbinom functions as expected!", {
  df <- data.frame(
    date = seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"),
    count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
  )
  df_nbinom <- alert_nbinom(df, t = date, y = count, baseline_end = as.Date("2020-03-01"))

  expect(is.data.frame(df_nbinom), failure_message = "alert_nbinom test fails!")
  expect_equal(length(df_nbinom), length(df) + 5)
})
