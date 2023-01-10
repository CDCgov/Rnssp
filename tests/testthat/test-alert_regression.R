context("test-alert_regression")

test_that("alert_regression functions as expected!", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    count = floor(runif(366, min = 0, max = 101))
  )

  df2 <- data.frame(
    date = rep(seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1), 2),
    regions = c(rep("reg1", 366), rep("reg2", 366)),
    count = floor(runif(366 * 2, min = 0, max = 101))
  )

  df_regression <- alert_regression(df)
  df2_regression <- alert_regression(group_by(df2, regions))

  expect(is.data.frame(df_regression), failure_message = "alert_regression test fails!")
  expect_equal(length(df_regression), length(df) + 6)
  expect(is.data.frame(df2_regression), failure_message = "alert_regression test fails!")
  expect_equal(length(df2_regression), length(df2) + 6)

  expect_error(alert_regression(df, B = 6))
  expect_error(alert_regression(df, B = 15))
  expect_error(alert_regression(df, g = -1))
  expect_error(alert_regression(df, B = 385))
  expect_error(alert_regression(df2))
})
