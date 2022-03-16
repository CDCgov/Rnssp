context("test-alert_farrington")

test_that("alert_farrington functions as expected!", {
  df <- data.frame(
    date = seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks"),
    count = rpois(length(seq(as.Date("2014-01-05"), as.Date("2022-02-05"), "weeks")), 25)
  )
  df_farr_original <- alert_farrington(df, t = date, y = count)
  df_farr_modified <- alert_farrington(df, t = date, y = count, method = "modified")

  expect(is.data.frame(df_farr_original), failure_message = "alert_farrington test fails for `method = 'original'`!")
  expect(is.data.frame(df_farr_modified), failure_message = "alert_farrington test fails for `method = 'modified'`!")
  expect_equal(length(df_farr_original), length(df) + 6)
  expect_equal(length(df_farr_modified), length(df) + 6)
})
