
#' @importFrom httr get_callback set_callback

.onLoad <- function(libname, pkgname) {
  debugme()

  if (!is.null(get_callback("request"))) {
    warning("httrmock removing another request callback")
  }
  set_callback("request", request_callback)

  if (!is.null(get_callback("response"))) {
    warning("httrmock removing another response callback")
  }
  set_callback("response", response_callback)
}

.onUnload <- function(libpath) {
  set_callback("request", NULL)
  set_callback("response", NULL)
}
