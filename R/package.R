
#' Mock HTTP Requests for API Testing
#'
#' @section Introduction:
#'
#' TODO
#' 
#' `httrmock` was inspired by https://github.com/assaf/node-replay
#'
#' @section Usage in packages:
#'
#' TODO
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
#' @family HTTP mocking
NULL

#' @importFrom storr storr_rds
#' @importFrom debugme debugme

get_storr <- function() {
  storrpath <- Sys.getenv("HTTRMOCK_STORE", "")
  if (storrpath == "") {
    storrpath <- file.path(get_test_root(), "httrmock")
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

#' @importFrom httr set_callback

stop_recording <- function() {
  "!DEBUG stop recording"
  set_callback("response", NULL)
  invisible()
}

stop_replaying <- function() {
  "!DEBUG stop replaying"
  set_callback("request", NULL)
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
  set_callback("response", recorder_function)
  invisible()
}

#' @importFrom whoami username

recorder_function <- function(req, res) {
  key <- request_key(req)
  ns  <- request_namespace(req)
  req <- filter_request(req)
  res <- filter_response(res)
  get_storr()$set(
    key,
    namespace = ns,
    list(
      request = req,
      response = res,
      user = username(fallback = "<unknown-user>"),
      timestamp = Sys.time()
    )
  )
  NULL
}

start_replaying <- function() {
  "!DEBUG Set up replaying"
  set_callback("request", replayer_function)
  invisible()
}

replayer_function <- function(req) {
  storr <- get_storr()
  key <- request_key(req)
  ns  <- request_namespace(req)
  if (storr$exists(key, namespace = ns)) {
    "!DEBUG Replay a request to '`req$url`'"
    storr$get(key, namespace = ns)$response
  } else {
    "!DEBUG Request `key` not found: '`req$url`', performing it"
    NULL
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
  nss <- storr$list_namespaces()
  keys <- unlist(
    lapply(nss, function(ns) storr$list(namespace = ns)),
    recursive = FALSE
  )
  vals <- unlist(
    lapply(nss, function(ns) lapply(keys, storr$get, namespace = ns)),
    recursive = FALSE
  )
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
  storr$clear(NULL)                     # all namespaces
  storr$gc()
  invisible()
}

#' @rdname clear_recordings
#' @param id Id of the recording to delete.
#' @export

del_recording <- function(id) {
  "!DEBUG Delete recording `id`"
  storr <- get_storr()
  ## We remove from eveywhere, it should be present once only, anyway
  nss <- storr$list_namespaces()
  for (ns in nss) {
    if (storr$exists(id, namespace = ns)) storr$del(id, namespace = ns)
  }
  storr$gc()
  invisible()
}

#' @importFrom httr get_callback

mocking_status <- function() {
  c(
    "recording" = identical(get_callback("response"), recorder_function),
    "replaying" = identical(get_callback("request"), replayer_function)
  )
}
