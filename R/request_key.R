
#' @importFrom digest digest

request_key <- function(req) {
  key <- drop_nulls(list(
    method = req$method,
    url = req$url,
    context = get_httrmock_context()
  ))
  digest(key)
}

filter_request <- function(req) {
  req$headers["Authorization"] <- "***** what are you looking for?"
  req$options$userpwd<-NULL
  req
}

filter_response <- function(resp) {
  resp$request <- filter_request(resp$request)
  resp
}
