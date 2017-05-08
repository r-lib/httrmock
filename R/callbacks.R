
#' @importFrom whoami username

response_callback <- function(req, res) {
  current <- within_current_test()

  print(current)

  ## if this is not the current test, then we do not record
  if (!current) return(NULL)

  ## for the current test, in 'record' mode we record
  if (ct_get_mode() != "record") return(NULL)

  key <- request_key(req)
  ns  <- request_namespace(req)
  req <- filter_request(req)
  res <- filter_response(res)

  msg("Recording request ", sQuote(key), " to ", sQuote(req$url))

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

  storr <- get_storr()
  key <- request_key(req)
  ns  <- request_namespace(req)
  if (storr$exists(key, namespace = ns)) {
    if (current && pwt() != "/") {
      msg("Playing ", sQuote(key), " to ", sQuote(req$url))
    }
    "!DEBUG Replay a request to '`req$url`'"
    storr$get(key, namespace = ns)$response
  } else {
    if (current && pwt() != "/") {
      msg("Cannot find ", sQuote(key), " to ", sQuote(req$url))
    }
    "!DEBUG Request `key` not found: '`req$url`', performing it"
    NULL
  }
}
