
context("current test")

test_that("mocking without recorded test", {

  skip_if_offline()
  skip_on_cran()

  expect_output(
    with_ct(
      mode = "mock",
      path = "/test-current-test.R",
      httr::GET("httpbin.org/?not_recorded")
    ),
    "Cannot find .*httpbin.org"
  )
})

test_that("mocking with recorded test", {

  res <- httr::GET("httpbin.org/?recorded")
  expect_equal(as.numeric(res$date), 1494174142)
})

test_that("message while mocking", {

  expect_output(
    with_ct(
      mode = "mock",
      path = "/test-current-test.R",
      httr::GET("httpbin.org/?recorded")
    ),
    "Playing .*httpbin.org"
  )
})
