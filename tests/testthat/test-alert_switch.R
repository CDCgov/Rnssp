context("test-alert_switch")

test_that("alert_switch functions as expected!", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    count = floor(runif(366, min = 0, max = 101))
  )

  df2 <- data.frame(
    date = rep(seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1), 2),
    regions = c(rep("reg1", 366), rep("reg2", 366)),
    count = floor(runif(366 * 2, min = 0, max = 101))
  )

  df_switch <- alert_switch(df)
  df2_switch <- alert_switch(group_by(df2, regions))

  expect(is.data.frame(df_switch), failure_message = "alert_switch test fails!")
  expect_equal(length(df_switch), length(df) + 5)
  expect(is.data.frame(df2_switch), failure_message = "alert_switch test fails!")
  expect_equal(length(df2_switch), length(df2) + 5)

  expect_error(alert_switch(df, B = 6))
  expect_error(alert_switch(df, B = 15))
  expect_error(alert_switch(df, g = -1))
  expect_error(alert_switch(df, B = 385))
})
