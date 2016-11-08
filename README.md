


# httrmock

> Mock HTTP Requests for API Testing

[![Linux Build Status](https://travis-ci.org/gaborcsardi/httrmock.svg?branch=master)](https://travis-ci.org/gaborcsardi/httrmock)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/httrmock?svg=true)](https://ci.appveyor.com/project/gaborcsardi/httrmock)
[![](http://www.r-pkg.org/badges/version/httrmock)](http://www.r-pkg.org/pkg/httrmock)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/httrmock)](http://www.r-pkg.org/pkg/httrmock)


Mock HTTP Requests for API Testing

## Installation


```r
devtools::install_github("gaborcsardi/httrmock")
```

## Introduction

Help make web API tests fast and reliable. The package operates
in two modes: in record mode, it performs HTTP requests and records
the responses in a database. In replay mode, it uses the recorded
responses to perform the tests without calling making HTTP requests.

`httrmock` is suitable for packages that use the `httr` package to
make HTTP queries.

It operates in two modes:
* In _recording_ mode, it intercepts all HTTP requests, and saves
  both the request and the response to it in an internal database.
  (More about the database later.)
* In _replaying_ mode, it intercepts the HTTP requests, and checks if
  they are in its internal database. If a request is found in the
  internal database, then it does not perform it, only returns the
  corresponding response from the database. If the request is not in the
  database, then it performs it.

`httrmock` was inspired by https://github.com/assaf/node-replay

## Usage in packages:

To use `httrmock` with `testthat` tests in a package, use the
following code in `tests/testthat.R`:

```r
library(testthat)
library(<yourpackage>)
httrmock::start_replaying()
test_check("<yourpackage>")
httrmock::stop_replaying()
```

This runs the tests during `R CMD check` with replay mode turned on,
and assumes that you already recorded the requests.

Running the tests via `devtools::test` unfortunately ignores this
file, so while developing the package with devtools, you need to
turn on or off the recording or replaying manually.

The following seems to be a sensible workflow:

1. Turn off recording and replaying.
2. Write all tests, and make sure they pass.
3. Remove all recorded requests and responses with `clear_recordings()`.
4. Turn on recording with `start_recording()`.
5. Run all the tests to record the requests.
6. Turn off recording, turn on replaying.
7. Make sure all tests still pass.
8. Try running `R CMD check` or `devtools::check()` on the package
   as a final test.

## The database:

`httrmock` uses the `storr` package to store the requests and responses
in RDS files, see `storr::storr()`. By default the database is in the
`tests/testthat` directory, which is appropriate for `testthat` tests.
For an alternative directory, set the the `HTTRMOCK_STORE` environment
variable and point it to the directory you wish to use. The directory
will be created if it does not exist.

## Debugging:

`httrmock` uses `debugme` for easy debugging of what is recorded and
replayed, see `debugme::debugme()` for details.

## License

MIT © Gábor Csárdi
