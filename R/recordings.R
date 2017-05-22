
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
  get_store()$list()
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
  get_store()$clear()
  invisible()
}

#' @rdname clear_recordings
#' @param id Id of the recording to delete.
#' @export

del_recording <- function(id) {
  "!DEBUG Delete recording `id`"
  get_store()$delete(id)
}
