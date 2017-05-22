
`%||%` <- function(l, r) if (is.null(l)) r else l

random_string <- function(length = 7) {
  paste(
    sample(c(letters, 0:9), length, replace = TRUE),
    collapse = ""
  )
}

named_lapply <- function(X, FUN, ...) {
  result <- lapply(X, FUN, ...)

  if (!is.null(names(X))) {
    names(result) <- names(X)

  } else if (is.character(X)) {
    names(result) <- X
  }

  result
}

read_until_blank_line <- function(path) {

  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)

  l <- readLines(con, n = 20)
  while (all(l != "")) {
    nl <- readLines(con, n = 20)
    if (!length(nl)) {
      stop("Internal error reading, no blank line found in ",
           sQuote(path))
    }
    l <- c(l, nl)
  }

  head(l, which(l == "")[1] - 1)
}

elt_or_na <- function(x, i) {
  x[[i]] %||% NA_character_
}
