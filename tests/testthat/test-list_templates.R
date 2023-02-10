context("test-list_templates")

test_that("list_templates() works!", {
  list_templates()
  expect_type(list_templates(), "character")
  expect_vector(list_templates())
  expect_type(list_templates(as.table = TRUE), "list")
  expect_s3_class(list_templates(as.table = TRUE), "data.frame")
  expect_s3_class(list_templates(as.table = TRUE), "tbl_df")
  expect_s3_class(list_templates(as.table = TRUE), "tbl")
})
