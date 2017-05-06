
#' @importFrom rprojroot find_root is_testthat is_r_package

get_test_root <- function() {
  find_root(is_testthat, find_root(is_r_package))
}

list_test_files <- function() {
  root <- get_test_root()
  dir(root, pattern = "^test")
}

#' @importFrom utils getParseData
#' @importFrom lintr get_source_expressions

list_test_blocks <- function(test_file) {
  test_path <- file.path(get_test_root(), test_file)

  ## This is not a mistake, but a workaround for a macOS bug
  exprs <- get_source_expressions(test_path)
  exprs <- get_source_expressions(test_path)$expressions

  ## test_that calls
  ttc <- Filter(
    function(x) parse(text = x$content)[[1]][[1]] == quote(test_that),
    exprs
  )

  ## eval them
  names <- unlist(lapply(
    ttc,
    function(x) {
      eval(
        parse(text = x$content),
        list(test_that = function(n, c) n)
      )
    }
  ))

  lines <- vapply(ttc, function(x) x$line, integer(1))
  data.frame(
    stringsAsFactors = FALSE,
    name = names,
    line = lines
  )
}
