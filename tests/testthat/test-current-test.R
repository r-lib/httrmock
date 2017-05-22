
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

  res <- with_ct(
    mode = "mock",
    path = "/test-current-test.R",
    httr::GET("httpbin.org/?recorded")
  )
  expect_equal(res$date, as.Date("2017-05-22"))
})
