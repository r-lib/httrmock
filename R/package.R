
#' Mock HTTP Requests for API Testing
#'
#' @section Introduction:
#'
#' TODO
#' 
#' `httrmock` was inspired by https://github.com/assaf/node-replay
#'
#' @section Usage in packages:
#'
#' TODO
#'
#' @section The database:
#'
#' `httrmock` uses the `storr` package to store the requests and responses
#' in RDS files, see [storr::storr()]. By default the database is in the
#' `tests/testthat` directory, which is appropriate for `testthat` tests.
#' For an alternative directory, set the the `HTTRMOCK_STORE` environment
#' variable and point it to the directory you wish to use. The directory
#' will be created if it does not exist.
#'
#' @section Debugging:
#'
#' `httrmock` uses `debugme` for easy debugging of what is recorded and
#' replayed, see [debugme::debugme()] for details.
#'
#' @docType package
#' @name httrmock
#' @family HTTP mocking
NULL
