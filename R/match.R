
is_matching_request <- function(req, rec) {
  is_matching_method(req$method, rec$method) &&
    is_matching_url(req$url, rec$url) &&
    is_matching_headers(req$headers, rec$headers)
}

is_matching_method <- function(req_method, rec_method) {
  is.null(rec_method) || grepl(rec_method, req_method, perl = TRUE)
}

is_matching_url <- function(req_url, rec_url) {
  is.null(rec_url) || req_url == rec_url ||
    is_matching_regex(req_url, rec_url)
}

is_matching_headers <- function(req_headers, rec_headers) {
  for (h in names(rec_headers)) {
    if (! h %in% names(req_headers)) return(FALSE)
    if (req_headers[[h]] != rec_headers[[h]] &&
        ! is_matching_headers(req_headers[[h]], rec_headers[[h]])) {
      return(FALSE)
    }
  }
  TRUE
}

is_matching_regex <- function(req_x, rec_x) {
  l <- nchar(rec_x)
  substr(rec_x, 1, 1) == "/" && substr(rec_x, l, l) == "/" &&
    grepl(substr(rec_x, 2, l - 1), req_x, perl = TRUE)
}
