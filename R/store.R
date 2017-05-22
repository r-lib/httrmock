
#' @importFrom debugme debugme

get_store <- function() {
  storepath <- Sys.getenv("HTTRMOCK_STORE", "")
  if (storepath == "") {
    storepath <- file.path(get_test_root(), "httrmock")
  }
  "!DEBUG Getting HTTP data from '`storepath`'"
  httr_store$new(storepath)
}

#' @importFrom R6 R6Class

httr_store <- R6Class(
  "httr_store",
  public = list(
    initialize = function(path) hstore_init(self, private, path),
    match = function(request) hstore_match(self, private, request),
    exists = function(request) hstore_exists(self, private, request),
    set = function(request, response, user)
      hstore_set(self, private, request, response, user),
    list = function() hstore_list(self, private),
    clear = function(force = FALSE) hstore_clear(self, private, force),
    delete = function(ids) store_delete(self, private, ids),
    refresh_index = function() hstore_refresh_index(self, private)
  ),
  private = list(
    path = NULL,
    index = NULL,
    recording_name = function(request)
      hstore__recording_name(self, private, request),
    new_filename = function(request, response, user)
      hstore__new_filename(self, private, request, response, user),
    list_filenames = function()
      hstore__list_filenames(self, private),
    write_recording = function(file, request, response, user)
      hstore__write_recording(self, private, file, request, response, user)
  )
)

hstore_init <- function(self, private, path) {
  private$path <- path
  self$refresh_index()
  invisible(self)
}

hstore_get <- function(self, private, id) {
  m <- grepl(id, names(private$index))
  if (any(m)) {
    private$index[m][[1]]
  } else {
    NULL
  }
}

hstore_match <- function(self, private, request) {
  for (recname in names(private$index)) {
    if (is_matching_request(request, private$index[[recname]])) {
      return(hstore__load_recording(recname))
    }
  }
  NULL
}

hstore_exists <- function(self, private, request) {
  for (rec in private$index) {
    if (is_matching_request(request, rec)) return(TRUE)
  }
  FALSE
}

hstore_set <- function(self, private, request, response, user) {
  ## TODO: check for matching recording first, to overwrite
  file <- private$new_filename(request, response, user)
  private$write_recording(file, request, response, user)
  ## TODO: refresh index?
  invisible(self)
}

hstore_list <- function(self, private) {
  ids <- names(private$index)
  df <- data.frame(
    stringsAsFactors = FALSE,
    id = hstore__id_from_filename(ids),
    verb = vapply(private$index, elt_or_na, character(1), "method"),
    url = vapply(private$index, elt_or_na, character(1), "url")
  )
  rownames(df) <- NULL
  df
}

hstore_clear <- function(self, private, force) {
  ## TODO
  stop("clear method is not implemented yet (just delete the files)")
}

hstore_delete <- function(self, private, ids) {
  ## TODO
  stop("delete method is not implemented yet (just delete the file)")
}

hstore_refresh_index <- function(self, private) {
  recfiles <- private$list_filenames()
  private$index <- named_lapply(recfiles, hstore__get_recording_header)
  invisible(self)
}

## -------------------------------------------------------------------

hstore__recording_name <- function(self, private, request) {
  urlname <- gsub("[^a-zA-Z0-9]+", "-", request$url)
  paste0(request$method, "-", urlname)
}

hstore__new_filename <- function(self, private, request, response, user) {
  file.path(
    private$path,
    paste0(
      private$recording_name(request),
      "-",
      random_string(),
      ".rec"
    )
  )
}

hstore__list_filenames <- function(self, private) {
  dir(
    path = private$path,
    pattern = "\\.rec$",
    recursive = TRUE,
    full.names = TRUE
  )
}

#' @importFrom glue glue
#' @importFrom yaml as.yaml

hstore__write_recording <- function(self, private, file, request,
                                    response, user) {

  request <- make_request_safe(request)
  response <- make_response_safe(response)

  on.exit(close(con), add = TRUE)
  con <- file(file, open = "w")

  treq <- transform_request_for_recording(request)
  tresp <- transform_response_for_recording(response)

  cat(file = con, glue("
    # Request parameters, these are used for request matching
    { as.yaml(treq) }

    # Response headers, extracted for readability
    { as.yaml(tresp$header) }

    # The rest of the response object
    BASE64 encoded response:
    { tresp$rest }

    # Finally, the content
    { tresp$content }
  "))
}

make_request_safe <- function(request) {
  ## TODO
  request
}

make_response_safe <- function(response) {
  response$request <- make_request_safe(response$request)
  response
}

#' Transform a Request for Recording
#'
#' We will not use the result to (re)create the request, we only use it
#' for matching, so the transformation does not have to be lossless.
#'
#' @param request The httr request object.
#' @return The transformed request, that can be written to a file easily.
#'
#' @keywords internal

transform_request_for_recording <- function(request) {
  list(
    method = request$method,
    url = request$url,
    headers = as.list(request$headers)
  )
}

#' @importFrom base64enc base64encode

transform_response_for_recording <- function(response) {

  show_elements <- c("url", "headers", "all_headers", "status_code",
                     "content", "cookies", "date")
  rest_elements <- setdiff(names(response), show_elements)

  list(
    header = list(
      url = response$url,
      headers = response$all_headers,
      cookies = response$cookies
    ),
    content = transform_response_content_for_recording(response),
    rest = base64encode(
      serialize(response[rest_elements], connection = NULL),
      linewidth = 60,
      newline = "\n"
    )
  )
}

#' @importFrom httr content

transform_response_content_for_recording <- function(response) {

  if ("content-type" %in% names(response$headers) &&
      is_plain_content_type(response$headers[["content-type"]])) {
    paste0("Content:\n", content(response, as = "text"))

  } else {
    paste0(
      "BASE64 encoded content:\n",
      base64encode(response$content, linewidth = 70, newline = "\n")
    )
  }
}

is_plain_content_type <- function(type) {

  plain <- c(
    "application/javascript",
    "application/json",
    "application/xml",
    "text/css",
    "text/html",
    "text/plain")

  type %in% plain || grepl(paste0(plain, "\\s*;"), type)
}

#' @importFrom yaml yaml.load

hstore__get_recording_header <- function(path) {
  lines <- read_until_blank_line(path)
  str <- paste(lines, collapse = "\n")
  yaml.load(str)
}

#' @importFrom tools file_path_sans_ext

hstore__id_from_filename <- function(files) {
  names <- file_path_sans_ext(basename(files))
  vapply(strsplit(names, "-"), tail, character(1), 1)
}

#' @importFrom base64enc base64decode

hstore__load_recording <- function(path) {
  lines <- readLines(path, warn = FALSE)
  i <- 1

  ## Find the first empty line, the end of the header
  while (lines[i] != "") i <- i + 1
  req <- yaml.load(paste(lines[1:i], collapse = "\n"))

  ## Skip empty lines, beginning of response override
  while (lines[i] == "") i <- i + 1
  j <- i

  ## Find next empty line, end of response override
  while (lines[i] != "") i <- i + 1
  resp_override <- yaml.load(paste(lines[j:i], collapse = "\n"))

  ## Skip empty lines, beginning of response
  while (lines[i] == "") i <- i + 1
  j <- i

  ## Find next empty line, end of response
  while (lines[i] != "") i <- i + 1
  resp_lines <- grep("^#", lines[(j+1):i], invert = TRUE, value = TRUE)
  resp <- unserialize(base64decode(paste(resp_lines[-1], collapse = "\n")))

  ## Skip empty lines
  while (lines[i] == "") i <- i + 1
  content_lines <- grep("^#", lines[i:length(lines)],
                         invert = TRUE, value = TRUE)
  base64 <- grepl("BASE64 encoded content:", content_lines[1])
  content <- if (base64) {
    base64decode(content_lines[-1])
  } else {
    charToRaw(paste(content_lines[-1], collapse = "\n"))
  }

  resp$url <- resp_override$url
  resp$status_code <- tail(resp_override$headers, 1)[[1]]$status
  resp$headers <- tail(resp_override$headers, 1)[[1]]$headers
  resp$all_headers <- resp_override$headers
  resp$cookies <- cookies_as_df(resp_override$cookies)
  resp$content <- content
  resp$date <- resp_override$date %||% Sys.Date()

  list(
    path = path,
    id = hstore__id_from_filename(path),
    response = structure(resp, class = "response"),
    request_matching = req,
    response_override = resp_override
  )
}

cookies_as_df <- function(cookies) {
  ## TODO
  cookies
}
