
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
  nss <- setdiff(storr$list_namespaces(), "objects")
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
