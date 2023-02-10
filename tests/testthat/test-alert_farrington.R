context("test-alert_farrington")

test_that("alert_farrington functions as expected!", {
  df <- data.frame(
    date = seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"),
    count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
  )

  df2 <- data.frame(
    date = rep(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"), 2),
    regions = c(rep("reg1", 422), rep("reg2", 422)),
    count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
  )

  df3 <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    count = floor(runif(366, min = 0, max = 101))
  )


  df_farr_original <- alert_farrington(df)
  df_farr_modified <- alert_farrington(df, method = "modified")

  df2_farr_original <- alert_farrington(group_by(df2, regions))
  df2_farr_modified <- alert_farrington(group_by(df2, regions), method = "modified")

  expect(is.data.frame(df_farr_original), failure_message = "alert_farrington test fails for `method = 'original'`!")
  expect(is.data.frame(df_farr_modified), failure_message = "alert_farrington test fails for `method = 'modified'`!")
  expect(is.data.frame(df2_farr_original), failure_message = "alert_farrington test fails for `method = 'original'`!")
  expect(is.data.frame(df2_farr_modified), failure_message = "alert_farrington test fails for `method = 'modified'`!")
  expect_equal(length(df_farr_original), length(df) + 6)
  expect_equal(length(df_farr_modified), length(df) + 6)
  expect_equal(length(df2_farr_original), length(df2) + 6)
  expect_equal(length(df2_farr_modified), length(df2) + 6)

  expect_error(alert_farrington(list(10)))
  expect_error(alert_farrington(df, B = 3))
  expect_error(alert_farrington(df, B = 1000))
  expect_error(alert_farrington(df, w = -1))
  expect_error(alert_farrington(df, method = "notamethod"))
  expect_error(alert_farrington(df2))
  expect_error(alert_farrington(df3))
  expect_error(alert_farrington(group_by(df2, regions), method = "notamethod"))
})
