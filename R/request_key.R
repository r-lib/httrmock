
#' @importFrom digest digest

request_key <- function(req) {
  key <- drop_nulls(list(
    method = req$method,
    url = req$url
  ))
  digest(key)
}

request_namespace <- function(req) {
  paste0(req$method, ":", url_to_ns(req$url))
}

url_to_ns <- function(url) {
  gsub("[^a-zA-Z0-9]+", "-", url)
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
