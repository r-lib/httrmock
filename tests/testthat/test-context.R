
context("context")

old <- Sys.getenv("HTTRMOCK_STORE", NA_character_)
on.exit(if (!is.na(old)) Sys.setenv("HTTRMOCK_STORE" = old))
Sys.setenv("HTTRMOCK_STORE" = tempfile())

test_that("context is used correctly", {

  skip_if_offline()
  skip_on_cran()

  ## Record a response
  start_recording()
  stop_replaying()
  resp1 <- httr::GET("https://httpbin.org/headers")

  ## We should not use this in the next query, which should return
  ## and record a different response
  start_replaying()
  start_recording()
  resp2 <- with_httrmock_context(
    "foobar",
    httr::GET(
      "https://httpbin.org/headers",
      httr::add_headers("X-Foobar" = "really")
    )
  )

  cnt1 <- jsonlite::fromJSON(httr::content(resp1, as = "text"))
  cnt2 <- jsonlite::fromJSON(httr::content(resp2, as = "text"))
  expect_false(identical(names(cnt1$headers), names(cnt2$headers)))

  ## If we use the right context, then the reply is replayed,
  ## and matches the recorded one. To make sure that no request
  ## is performed here, we set the header to a different value.
  start_replaying()
  stop_recording()
  resp3 <- with_httrmock_context(
    "foobar",
    httr::GET(
      "https://httpbin.org/headers",
      httr::add_headers("X-Foobar" = "not-really")
    )
  )

  cnt3 <- jsonlite::fromJSON(httr::content(resp3, as = "text"))
  expect_identical(cnt2, cnt3)

  ## And is we replay in the default context, we get the first
  ## recorded value. Again, we set a header, to make sure that the
  ## request was not performed.
  start_replaying()
  stop_recording()
  resp4 <- httr::GET(
    "https://httpbin.org/headers",
    httr::add_headers("X-Foobar" = "this-time-really")
  )

  cnt4 <- jsonlite::fromJSON(httr::content(resp4, as = "text"))
  expect_identical(cnt4, cnt1)
})
