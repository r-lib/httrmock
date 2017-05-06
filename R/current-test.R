
#' About the current tests
#'
#' See [ct()] before you read this. This documentation is for `httrmock`
#' developers.
#'
#' The state of the current tests are stored in environment variables:
#' * `HTTRMOCK_CURRENT_TEST` contains the current test. Possible values:
#'    * `/`: the test directory is selected. This is the default value
#'      when the environment variable is not set.
#'    * Name of the test file that is selected, e.g. `test-filename.R`.
#'    * Name of the test file and test block that is selected, e.g.
#'      `test-filename.R/test-block-name`.
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
#'   * the (unique) beginning of the name of a test file, or, if the
#'     current test is a test file, the (unique) beginning of a test block
#'     name.
#'   * `".."`, in which case we step *out*, i.e. from a test block to the
#'     test file, from a test file to the test directory including all
#'     tests.
#'   * `"/"`, to select the test directory including all tests.
#'
#' @export
#' @family test selection functions
#' @examples
#' TODO

ct <- function(x = "") {
  wt <- pwt()
  newwt <- if (x == ".." && wt == "/") {
    wt
  } else if (x == ".." && grepl("/", wt)) {
    sub("/.*$", "", wt)

  } else if (x == "..") {
    "/"

  } else if (x == "/") {
    "/"

  } else if (wt == "/") {
    ct_into_file(wt, x)

  } else {
    ct_into_block(wt, x)
  }

  Sys.setenv("HTTRMOCK_CURRENT_TEST" = newwt)
  invisible(pwt())
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

  if (!is.na(pm)) {
    files[pm]

  } else if (length(files) == 1 && file == "") {
    files

  } else if (interactive()) {
    choose(file, files)

  } else {
    stop("Cannot set current test file, ", sQuote(file), " is ambigouos")
  }
}

#' Step into a test block
#'
#' @param current Current test.
#' @param block Block name or beginning of the name
#'   (possible empty string).
#'
#' @keywords internal

ct_into_block <- function(current, block) {
  if (grepl("/", current)) stop("Already in a block")

  blocks <- list_test_blocks(current)$name
  pm <- pmatch(block, blocks)

  newblock <- if (!is.na(pm)) {
    blocks[pm]

  } else if (length(blocks) == 1 && block == "") {
    blocks

  } else if (interactive() && ! grepl("/", current)) {
    choose(block, blocks)

  } else {
    stop("Cannot set current test block, ", sQuote(block), " is ambigouos")
  }

  paste0(current, "/", newblock)
}

#' Get the current working test
#'
#' See [ct()] for a better description of current working tests.
#'
#' `getwt` (get working test) is a synonym of `pwt` (print working test).
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
#' @rdname ct

setwt <- ct

#' Set or query the mocking status of the current test
#'
#' Note that these settings only apply to the *current test*. Other
#' test cases are always mocked.
#'
#' `ct_mode` queries the current state.
#'
#' `ct_nomock` turns off mocking. for the current test. You typically want
#' this while you are writing or extending the test cases. `httrmock` goes
#' completely out of the way, when this mode is selected.
#'
#' `ct_record` turns on recording mode. All HTTP responses are recorded
#' in this mode.
#'
#' `ct_mock` turns on mocking mode. HTTP requests are not performed,
#' the recorded responses are replayed. This is the default state after
#' you have changed the current test.
#'
#' @return `ct_mode` returns `"mock"`, `"nomock"` or `"record"`.
#'
#' @export

ct_mode <- function() {
  Sys.getenv("HTTRMOCK_CURRENT_TEST_MODE", "mock")
  ## TODO: check value
}

#' @export
#' @rdname ct_mode

ct_nomock <- function() {
  Sys.setenv("HTTRMOCK_CURRENT_TEST_MODE" = "nomock")
}

#' @export
#' @rdname ct_mode

ct_record <- function() {
  Sys.setenv("HTTRMOCK_CURRENT_TEST_MODE" = "record")
}

#' @export
#' @rdname ct_mode

ct_mock <- function() {
  Sys.setenv("HTTRMOCK_CURRENT_TEST_MODE" = "mock")
}
