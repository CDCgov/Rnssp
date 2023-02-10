context("test-alert_nbinom")

test_that("alert_nbinom functions as expected!", {
  df <- data.frame(
    date = seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"),
    count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
  )

  df2 <- data.frame(
    date = rep(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"), 2),
    regions = c(rep("reg1", 422), rep("reg2", 422)),
    count = floor(runif(422 * 2, min = 0, max = 101))
  )

  df3 <- data.frame(
    date = rep(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"), 2),
    regions = c(rep("reg1", 422), rep("reg2", 422)),
    count = floor(runif(422 * 2, min = -100, max = 100))
  )

  df_nbinom <- alert_nbinom(df, baseline_end = as.Date("2020-03-01"))
  df2_nbinom <- alert_nbinom(group_by(df2, regions),
                             baseline_end = as.Date("2020-03-01"), include_time = FALSE)

  expect(is.data.frame(df_nbinom), failure_message = "alert_nbinom test fails!")
  expect_equal(length(df_nbinom), length(df) + 5)
  expect(is.data.frame(df2_nbinom), failure_message = "alert_nbinom test fails!")
  expect_equal(length(df2_nbinom), length(df2) + 5)

  expect_error(alert_nbinom(df, baseline_end = as.Date("2014-01-31")))
  expect_error(alert_nbinom(df2, baseline_end = as.Date("2020-03-01")))
  expect_error(alert_nbinom(df3, baseline_end = as.Date("2020-03-01")))
})
