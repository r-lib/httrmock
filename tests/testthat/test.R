
context("httrmock")

old <- Sys.getenv("HTTRMOCK_STORE", NA_character_)
on.exit(if (!is.na(old)) Sys.setenv("HTTRMOCK_STORE" = old))
Sys.setenv("HTTRMOCK_STORE" = tempfile())

test_that("httrmock works", {

  skip_if_offline()
  skip_on_cran()

  start_recording()
  resp1 <- httr::GET("httpbin.org")
  stop_recording()

  rec <- list_recordings()
  expect_equal(nrow(rec), 1)
  expect_equal(rec$url, "httpbin.org")

  start_replaying()
  resp2 <- httr::GET("httpbin.org")
  stop_replaying()

  expect_equal(filter_response(resp1), resp2)

})

test_that("POST", {

  skip_if_offline()
  skip_on_cran()

  start_recording()
  resp1 <- httr::POST(
    "httpbin.org/post",
    body = "{ \"foo\": 42 }",
    encode = "json"
  )
  stop_recording()

  start_replaying()
  resp2 <- httr::POST(
    "httpbin.org/post",
    body = "{ \"foo\": 42 }",
    encode = "json"
  )
  stop_replaying()

  expect_equal(filter_response(resp1), resp2)

  json <- jsonlite::fromJSON(httr::content(resp2, as = "text"))
  expect_equal(json$data, "{ \"foo\": 42 }")
})
