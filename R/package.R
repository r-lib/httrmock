
#' Mock HTTP Requests for API Testing
#'
#' @section Introduction:
#'
#' Help make web API tests fast and reliable. The package operates
#' in two modes: in record mode, it performs HTTP requests and records
#' the responses in a database. In replay mode, it uses the recorded
#' responses to perform the tests without calling making HTTP requests.
#'
#' `httrmock` is suitable for packages that use the `httr` package to
#' make HTTP queries.
#'
#' It operates in two modes:
#' * In _recording_ mode, it intercepts all HTTP requests, and saves
#'   both the request and the response to it in an internal database.
#'   (More about the database later.)
#' * In _replaying_ mode, it intercepts the HTTP requests, and checks if
#'   they are in its internal database. If a request is found in the
#'   internal database, then it does not perform it, only returns the
#'   corresponding response from the database. If the request is not in the
#'   database, then it performs it.
#'
#' `httrmock` was inspired by https://github.com/assaf/node-replay
#'
#' @section Usage in packages:
#'
#' To use `httrmock` with `testthat` tests in a package, use the
#' following code in `tests/testthat.R`:
#'
#' ```r
#' library(testthat)
#' library(<yourpackage>)
#' httrmock::start_replaying()
#' test_check("<yourpackage>")
#' httrmock::stop_replaying()
#' ```
#'
#' This runs the tests during `R CMD check` with replay mode turned on,
#' and assumes that you already recorded the requests.
#'
#' Running the tests via `devtools::test` unfortunately ignores this
#' file, so while developing the package with devtools, you need to
#' turn on or off the recording or replaying manually.
#'
#' The following seems to be a sensible workflow:
#'
#' 1. Turn off recording and replaying.
#' 2. Write all tests, and make sure they pass.
#' 3. Remove all recorded requests and responses with [clear_recordings()].
#' 4. Turn on recording with [start_recording()].
#' 5. Run all the tests to record the requests.
#' 6. Turn off recording, turn on replaying.
#' 7. Make sure all tests still pass.
#' 8. Try running `R CMD check` or `devtools::check()` on the package
#'    as a final test.
#'
#' @section The database:
#'
#' `httrmock` uses the `storr` package to store the requests and responses
#' in RDS files, see [storr::storr()]. By default the database is in the
#' `tests/testthat` directory, which is appropriate for `testthat` tests.
#' For an alternative directory, set the the `HTTRMOCK_STORE` environment
#' variable and point it to the directory you wish to use. The directory
#' will be created if it does not exist.
#'
#' @section Debugging:
#'
#' `httrmock` uses `debugme` for easy debugging of what is recorded and
#' replayed, see [debugme::debugme()] for details.
#'
#' @docType package
#' @name httrmock
#' @importFrom httr GET
#' @family HTTP mocking
NULL

tmp <- new.env()
tmp$req <- NULL
tmp$res <- NULL

#' @importFrom rprojroot find_root is_testthat
#' @importFrom storr storr_rds
#' @importFrom debugme debugme

get_storr <- function() {
  storrpath <- Sys.getenv("HTTRMOCK_STORE", "")
  if (storrpath == "") {
    storrpath <- file.path(find_root(is_testthat), "httrmock")
  }
  "!DEBUG Getting HTTP data from '`storrpath`'"
  storr_rds(storrpath)
}

.onLoad <- function(libname, pkgname) {
  debugme()
}

.onUnload <- function(libpath) {
  stop_recording()
  stop_replaying()
}

#' @rdname start_recording
#' @export

stop_recording <- function() {
  "!DEBUG stop recording"
  untrace_call <- as.call(list(
    untrace,
    as.call(list(as.symbol(":::"), quote(httr), quote(request_perform)))
  ))
  try(suppressMessages(eval(untrace_call)), silent = TRUE)
  invisible()
}

#' @rdname start_recording
#' @export

stop_replaying <- function() {
  "!DEBUG stop replaying"
  untrace_call <- as.call(list(
    untrace,
    as.call(list(as.symbol(":::"), quote(httr), quote(request_perform)))
  ))
  try(suppressMessages(eval(untrace_call)), silent = TRUE)
  invisible()
}

#' Start or stop recording or replaying HTTP requests
#'
#' `start_recording()` starts recording, and `stop_recording()` stops it.
#'
#' @family HTTP mocking
#' @export

start_recording <- function() {
  "!DEBUG Set up recording"
  trace_call <- as.call(list(
    trace, as.call(list(as.symbol(":::"), quote(httr), quote(request_perform))),
    print = FALSE,
    tracer = record_tracer_function,
    exit = record_tracer_exit_function
  ))
  suppressMessages(eval(trace_call))
  invisible()
}

record_tracer_function <- function() {
  tmp$req <- get("req", envir = parent.frame())
  "!DEBUG Record a request to '`tmp$req$url`'"
}

#' @importFrom digest digest
#' @importFrom whoami username

record_tracer_exit_function <- function() {

  tmp$res <- returnValue()
  if (inherits(tmp$res, "response")) {
    "!DEBUG Record a response from '`tmp$res$url`'"
    req <- filter_request(tmp$req)
    res <- filter_response(tmp$res)
    get_storr()$set(
      digest(req),
      list(
        request = req,
        response = res,
        user = username(fallback = "<unknown-user>"),
        timestamp = Sys.time()
      )
    )

  } else {
    "!DEBUG Not recording response, error?"
    warning(
      "Not recording response from ",
      tmp$req$url,
      ", request failed?"
    )
  }
}

filter_request <- function(req) {
  if ("Authorization" %in% names(req$headers)) {
    req$headers["Authorization"] <- "***** what are you looking for?"
  }
  req
}

filter_response <- function(resp) {
  resp$request <- filter_request(resp$request)
  resp
}

#' @rdname start_recording
#' @importFrom utils packageVersion
#' @export

start_replaying <- function() {
  httr_version <- packageVersion("httr")
  if (httr_version == "1.2.1") {
    start_replaying_1.2.1()
  } else {
    warning("Need httr 1.2.1 for httrmock, not activating replay")
  }
}

start_replaying_1.2.1 <- function() {
  "!DEBUG Set up replaying (httr 1.2.1)"
  trace_call <- as.call(list(
    trace, as.call(list(as.symbol(":::"), quote(httr), quote(request_perform))),
    print = FALSE,
    tracer = replay_tracer_function
  ))
  suppressMessages(eval(trace_call))
  invisible()
}

replay_tracer_function <- function() {
  tmp$req <- get("req", envir = parent.frame())
  storr <- get_storr()
  key <- digest(filter_request(tmp$req))
  if (storr$exists(key)) {
    "!DEBUG Replay a request to '`tmp$req$url`'"
    assign(
      "request_fetch",
      function(...) {
        list(status_code = 200, headers = raw(0))
      },
      envir = parent.frame()
    )
    assign("refresh", FALSE, envir = parent.frame())
    assign(
          "response",
      function(...) {
        storr$get(key)$response
      },
      envir = parent.frame()
    )
  } else {
    "!DEBUG Request `key` not found: '`tmp$req$url`', performing it"
  }
}

#' List all HTTP request and response recordings
#'
#' @return A data frame with columns:
#' * `id` The id of the recording. You can use this to delete it with
#'   [del_recording()].
#' * `method` The HTTP method.
#' * `url` The URL used in the request. Note that the URL in the response
#'   might be different.
#' * `user` The local username of the user that recorded the request,
#'   as obtained via [whoami::username()].
#' * `timestamp` The exact date and time when the request was recorded.
#'
#' @export
#' @family HTTP mocking

list_recordings <- function() {
  "!DEBUG List recordings"
  storr <- get_storr()
  keys <- storr$list()
  vals <- lapply(keys, storr$get)
  df <- data.frame(
    stringsAsFactors = FALSE,
    id = keys,
    method = vapply(vals, function(x) x$request$method, ""),
    url = vapply(vals, function(x) x$request$url, ""),
    user = vapply(vals, "[[", "", "user"),
    timestamp = as.POSIXct(vapply(vals, function(x) as.character(x$timestamp), ""))
  )
  structure(df, class = c("httrmock_recordings", "data.frame"))
}

#' Delete one or all HTTP request recordings
#'
#' `del_recording` deletes a single recording. `clear_recordings` deletes
#' all of them in the current data store.
#'
#' They both use the data store in the `HTTRMOCK_STORE` environment
#' variable, or the default one if it is not set.
#'
#'
#' @export
#' @family HTTP mocking

clear_recordings <- function() {
  "!DEBUG Clear recordings"
  storr <- get_storr()
  storr$clear()
  storr$gc()
  invisible()
}

#' @rdname clear_recordings
#' @param id Id of the recording to delete.
#' @export

del_recording <- function(id) {
  "!DEBUG Delete recording `id`"
  storr <- get_storr()
  storr$del(id)
  storr$gc()
  invisible()
}
