context("test-askme")

test_that("askme() works!", {
  options(askme = function(...){
    'password'
  })
  expect_equal(askme(), NULL)
})
