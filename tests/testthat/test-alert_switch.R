context("test-alert_switch")

test_that("alert_switch functions as expected!", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    count = floor(runif(366, min = 0, max = 101))
  )
  df_switch <- alert_switch(df)

  expect(is.data.frame(df_switch), failure_message = "alert_switch test fails!")
  expect_equal(length(df_switch), length(df) + 5)
})
