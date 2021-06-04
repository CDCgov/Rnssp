context("test-change_dates")

test_that("change_dates functions as expected!", {
  url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/hospitalSyndromeAlerts?end_date=31Jan2021&start_date=29Jan2021"
  url1 <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/hospitalSyndromeAlerts?end_date=31Jan2021&start_date=15Jan2021"
  url2 <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/hospitalSyndromeAlerts?end_date=15Feb2021&start_date=29Jan2021"
  url3 <- "https://essence.syndromicsurveillance.org/nssp_essence/api/alerts/hospitalSyndromeAlerts?end_date=15Feb2021&start_date=15Jan2021"

  expect_equal(url1, change_dates(url, start_date = "2021-01-15"))
  expect_equal(url2, change_dates(url, end_date = "2021-02-15"))
  expect_equal(url3, change_dates(url, start_date = "2021-01-15", end_date = "2021-02-15"))
})
