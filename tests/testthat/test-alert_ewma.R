context("test-alert_ewma")

test_that("alert_ewma functions as expected!", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    count = floor(runif(366, min = 0, max = 101))
  )

  df2 <- data.frame(
    date = rep(seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1), 2),
    regions = c(rep("reg1", 366), rep("reg2", 366)),
    count = floor(runif(366 * 2, min = 0, max = 101))
  )

  df_ewma <- alert_ewma(df)
  df2_ewma <- alert_ewma(group_by(df2, regions))

  expect(is.data.frame(df_ewma), failure_message = "alert_ewma test fails!")
  expect_equal(length(df_ewma), length(df) + 4)
  expect(is.data.frame(df2_ewma), failure_message = "alert_ewma test fails!")
  expect_equal(length(df2_ewma), length(df2) + 4)

  expect_error(alert_ewma(df, B = 6))
  expect_error(alert_ewma(df, g = -1))
  expect_error(alert_ewma(df, B = 385))
  expect_error(alert_ewma(df2))
})
