
#' About the current tests
#'
#' See [ct()] before you read this. This documentation is for `httrmock`
#' developers.
#'
#' The state of the current tests are stored in environment variables:
#' * `HTTRMOCK_CURRENT_TEST` contains the current test. Possible values:
#'    * `/`: the test directory is selected. This is the default value
#'      when the environment variable is not set.
#'    * Name of the test file that is selected, e.g. `/test-filename.R`.
#'    * Name of the test file and test block that is selected, e.g.
#'      `/test-filename.R/test-block-name`.
#' * `HTTRMOCK_CURRENT_TEST_MODE` contains the current test mode. Possible
#'   values: `nomock`, `record` and `mock`. `mock` is the default value
#'   after you have changed the current test, and/or the environment
#'   variable is not set.
#'
#' @name current_test
#' @keywords internal
NULL

#' Change the current working test
#'
#' The current working test can be:
#' * all tests in the test directory,
#' * all tests in a test file,
#' * all expectation in a single test block.
#'
#' If `x` does not uniquely specify the test file or test block, and the
#' R session is interactive, then the user needs to select between them
#' manually. For non-interactive R sessions and error is thrown in this
#' case.
#'
#' `setwt` (set working test) is a synomym of `ct` (change test).
#'
#' @param x Sub-test to step into. It might be
#'   * `"/"`, to select the test directory including all tests.
#'   * the absolute "path" to a test file or test block.
#'   * the (unique) beginning of the name of a test file, or, if the
#'     current test is a test file, the (unique) beginning of a test block
#'     name.
#'   * `".."`, in which case we step *out*, i.e. from a test block to the
#'     test file, from a test file to the test directory including all
#'     tests.
#'
#' @export
#' @family test selection functions
#' @examples
#' TODO

ct <- function(x = "") {
  wt <- pwt()

  ## Absolute or relative path
  newct <- if (substr(x, 1, 1) == "/") {
    x

  } else if (x == "..") {
    ct_up(wt)

  } else{
    ct_relative(wt, x)
  }

  ## Check that it is valid
  ct_check_ct(newct)

  Sys.setenv("HTTRMOCK_CURRENT_TEST" = newct)
  invisible(pwt())
}

ct_up <- function(current) {
  dirname(current)
}

ct_relative <- function(current, new) {
  if (current == "/") {
    ct_into_file(current, new)

  } else {
    ct_into_block(current, new)
  }
}

#' Step into a test file
#'
#' @param current Current test.
#' @param file File name or beginning of the name (without the `test[-_]?`
#'   prefix), possibly empty string.

ct_into_file <- function(current, file) {
  files <- list_test_files()
  trimmed_files <- sub("^test[-_]?", "", files)
  pm <- pmatch(file, trimmed_files)

  newfile <- if (!is.na(pm)) {
    files[pm]

  } else if (length(files) == 1 && file == "") {
    files

  } else if (interactive()) {
    choose(file, files)

  } else {
    stop("Cannot set current test file, ", sQuote(file), " is ambigouos")
  }

  paste0("/", newfile)
}

#' Step into a test block
#'
#' @param current Current test.
#' @param block Block name or beginning of the name
#'   (possible empty string).
#'
#' @keywords internal

ct_into_block <- function(current, block) {

  test_file <- ct_get_test_file(current)

  blocks <- list_test_blocks(test_file)$name
  pm <- pmatch(block, blocks)

  newblock <- if (!is.na(pm)) {
    blocks[pm]

  } else if (length(blocks) == 1 && block == "") {
    blocks

  } else if (interactive()) {
    choose(block, blocks)

  } else {
    stop("Cannot set current test block, ", sQuote(block), " is ambigouos")
  }

  paste0(current, "/", newblock)
}

ct_check_ct <- function(x) {
  if (substr(x, 1, 1) != "/") {
    stop("Invalid path: ", sQuote(x), " should start with ", sQuote("/"))
  }
  if (x == "/") return()

  test_root <- get_test_root()
  test_file <- ct_get_test_file(x)
  test_block <- ct_get_test_block(x)

  if (!file.exists(file.path(test_root, test_file))) {
    stop("Test file does not exist: ", sQuote(test_file))
  }

  if (test_block == "") return()

  blocks <- list_test_blocks(test_file)
  if (! test_block %in% blocks$name) {
    stop("Test block ", sQuote(test_block), " does not exist in file ",
         sQuote(test_file))
  }
}

ct_get_test_file <- function(x) {
  if (x == "/") {
    ""
  } else if (dirname(x) == "/") {
    ## /file
    basename(x)

  } else {
    ## /file/block
    basename(dirname(x))
  }
}

ct_get_test_block <- function(x) {
  if (x == "/") {
    ""

  } else if (dirname(x) == "/") {
    ""

  } else {
    basename(x)
  }
}

#' Get the current working test
#'
#' See [ct()] for a better description of current working tests.
#'
#' `getwt` (get working test) and `pct` (print current test) are synonyms
#' of `pwt` (print working test).
#'
#' @return Path to a test direcory, test file, or path and name of the
#'   test block.
#'
#' @export
#' @family test selection functions
#' @examples
#' TODO

pwt <- function() {
  Sys.getenv("HTTRMOCK_CURRENT_TEST", "/")
  ## TODO: check that value makes sense
}

#' @export
#' @rdname pwt

getwt <- pwt

#' @export
#' @rdname pwt

pct <- pwt

#' @export
#' @rdname ct

setwt <- ct

#' Set or query the mocking status of the current test
#'
#' Note that these settings only apply to the *current test*. Other
#' test cases are always mocked.
#'
#' `ct_get_mode` queries the current mode.
#'
#' `ct_set_mode` sets the current mode.
#'
#' Modes:
#' * `nomock` turns off mocking. for the current test. You typically want
#' this while you are writing or extending the test cases. `httrmock` goes
#' completely out of the way, when this mode is selected.
#' * record` turns on recording mode. All HTTP responses are recorded
#' in this mode.
#' * `mock` turns on mocking mode. HTTP requests are not performed,
#' the recorded responses are replayed. This is the default state after
#' you have changed the current test.
#'
#' @return `ct_get_mode` returns `"mock"`, `"nomock"` or `"record"`.
#'
#' @export

ct_get_mode <- function() {
  Sys.getenv("HTTRMOCK_CURRENT_TEST_MODE", "mock")
}

ct_set_mode <- function(mode) {
  mode <- match.arg(mode, c("nomock", "record", "mock"))
  Sys.setenv("HTTRMOCK_CURRENT_TEST_MODE" = mode)
}

within_current_test <- function() {
  calls <- sys.calls()
  frames <- sys.frames()
  test_that_calls <- Filter(
    function(x) identical(calls[[x]][[1]], quote(test_that)),
    seq_along(calls)
  )
  last <- tail(test_that_calls, 1)

  if (!length(last)) return(FALSE)

  current <- pwt()
  if (current == "/") {
    TRUE

  } else {
    filename <- getSrcFilename(calls[[last]])
    blockname <- get("desc", frames[[last]])
    if (grepl("/.*/", current)) {
      paste0("/", filename, "/", blockname) == current
    } else {
      paste0("/", filename) == current
    }
  }
}

ct_stack <- new.env(parent = emptyenv())

ct_push <- function() {
  ct_stack$mode <- c(ct_stack$mode, ct_get_mode())
  ct_stack$path <- c(ct_stack$path, pct())
}

ct_pop <- function() {
  mode <- tail(ct_stack$mode, 1)
  path <- tail(ct_stack$path, 1)
  ct_stack$mode <- head(ct_stack$mode, -1)
  ct_stack$path <- head(ct_stack$path, -1)
  if (length(mode)) ct_set_mode(mode)
  if (length(path)) ct(path)
}

with_ct <- function(mode, expr, path = "/") {
  ct_push()
  on.exit(ct_pop())
  ct_set_mode(mode)
  ct(path)
  expr
}
