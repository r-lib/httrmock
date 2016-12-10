
data <- new.env(parent = emptyenv())

## Empty list must be treated specially

#' @importFrom utils tail

get_httrmock_context <- function() {
  ctx <- data$contexts
  if (length(ctx)) {
    tail(ctx, 1)[[1]]
  } else {
    NULL
  }
}

push_httrmock_context <- function(context) {
  data$contexts <- append(data$contexts, list(context))
}

pop_httrmock_context <- function() {
  ctx <- data$contexts
  if (! length(ctx)) stop("Cannot remove httr context, internal error")
  data$contexts <- tail(ctx, -1)
}

#' Perform `httrmock` request matching in a special context
#'
#' A request only matches a recorded request, if
#' * their HTTP method match,
#' * their URIs match, and
#' * their context match.
#'
#' You can (and need to, currently) create a context every time you are
#' requesting the same URL multiple times, and you expect different
#' responses, for example because you used difference HTTP headers.
#'
#' There is also a *default* context, this is used for expressions
#' outside of `with_httrmock_context` calls.
#'
#' @param context A string scalar, the name of the context.
#' @param expr An R expression to evaluate in the `httrmock` context.
#' @return The value of `expr`.
#' 
#' @export
#' @family HTTP mocking
#' @examples
#'
#' ## REcord to a temporary file
#' Sys.setenv("HTTRMOCK_STORE" = tempfile())
#'
#' ## Record a response, in the default context
#' start_recording()
#' stop_replaying()
#' resp1 <- httr::GET("https://httpbin.org/headers")
#'
#' ## Use the same URL again, in a different context, so the previously
#' ## recorded response is not replayed, and we get a different response.
#' start_replaying()
#' start_recording()
#' resp2 <- with_httrmock_context(
#'   "foobar",
#'   httr::GET(
#'     "https://httpbin.org/headers",
#'     httr::add_headers("X-Foobar" = "really")
#'   )
#' )
#'
#' jsonlite::fromJSON(httr::content(resp1, as = "text"))$headers
#' jsonlite::fromJSON(httr::content(resp2, as = "text"))$headers

with_httrmock_context <- function(context, expr) {
  assert_that(is_string(context))
  on.exit(pop_httrmock_context())
  push_httrmock_context(context)
  expr
}
