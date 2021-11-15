context("test-alert_mar")

test_that("alert_mar functions as expected!", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1),
    count = floor(runif(366, min = 0, max = 101))
  )
  df_mar <- alert_mar(df)

  expect(is.data.frame(df_mar), failure_message = "alert_mar test fails!")
  expect_equal(length(df_mar), length(df) + 3)
})
