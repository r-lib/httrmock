
#' @importFrom whoami username

response_callback <- function(req, res) {
  current <- within_current_test()

  ## if this is not the current test, then we do not record
  if (!current) return(NULL)

  ## for the current test, in 'record' mode we record
  if (ct_get_mode() != "record") return(NULL)

  msg("Recording request to ", sQuote(req$url))

  get_store()$set(
    req, res,
    user = username(fallback = "<unknown-user>")
  )
  NULL
}

request_callback <- function(req) {
  current <- within_current_test()
  mode <- ct_get_mode()

  ## For the current test we only replay if we are in mock mode
  if (current) {
    if (mode == "record" || mode == "nomock") {
      msg("Not playing ", sQuote(req$url), " in ", sQuote(mode), " mode")
      return(NULL)
    }
  }

  ## TODO: we should not re-create the store every time,
  ## because it takes a long time load all responses
  store <- get_store()
  recording <- store$match(req)
  if (! is.null(recording)) {
    if (current) msg("Playing ", sQuote(recording$id))
    "!DEBUG Replay '`recording$id`' a request to '`req$url`'"
    recording$response
  } else {
    if (current) msg("Cannot find recording to ", sQuote(req$url))
    "!DEBUG Request not found to '`req$url`', performing it"
    NULL
  }
}
