
#' @importFrom digest digest

request_key <- function(req) {
  digest(list(method = req$method, url = req$url))
}

filter_request <- function(req) {
  req$headers["Authorization"] <- "***** what are you looking for?"
  req
}

filter_response <- function(resp) {
  resp$request <- filter_request(resp$request)
  resp
}
