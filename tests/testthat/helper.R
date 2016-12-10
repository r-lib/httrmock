
## Clean up recorded requests. They are not meant to be used
## for the tests in this package, at least not for now.

if (basename(getwd()) == "testthat") unlink("httrmock")

is_online <- function(host = "httpbin.org", port = 80) {

  res <- tryCatch(
    pingr::ping_port(host, count = 1L, port = port),
    error = function(e) NA
  )

  !is.na(res)
}

skip_if_offline <- function() {
  if (!is_online()) skip("offline")
}
