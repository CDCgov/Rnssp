context("test-webscrape_icd")

test_that("webscrape_icd() function works!", {
  icd10_2021 <- webscrape_icd(icd_version = "ICD10", year = 2021, quiet = TRUE)
  icd10_2020 <- webscrape_icd(icd_version = "ICD10", year = 2020, quiet = TRUE)
  icd9_2014 <- webscrape_icd(icd_version = "ICD9", quiet = TRUE)
  expect_s3_class(icd10_2021, "data.frame")
  expect_s3_class(icd10_2020, "data.frame")
  expect_s3_class(icd9_2014, "data.frame")
  expect_error(webscrape_icd("icd10", quiet = TRUE), "ICD version argument")
  expect_error(webscrape_icd("ICD10", 2014, quiet = TRUE), "ICD-10 code sets prior to 2019 are not supported")
  expect_error(webscrape_icd("ICD9", 2020, quiet = TRUE), "only applies for ICD10")
})
