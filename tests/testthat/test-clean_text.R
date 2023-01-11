context("test-clean_text")

test_that("clean_text() function works!", {
  data <- data.frame(
    ChiefComplaintParsed = c("STOMACH PAIN", "FEVER", "COVID SYMPTOMS"),
    DischargeDiagnosis = c(";K12.30;U07.1;G89.29;", ";U07.1;", ";U07.1;")
  )

  clean_data <- clean_text(data)

  expect_s3_class(clean_data, "data.frame")
  expect_error(clean_text(list(1)))
  expect_error(clean_text(data.frame()))
})
