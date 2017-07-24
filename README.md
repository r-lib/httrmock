


# httrmock

> Mock HTTP Requests for API Testing

[![Linux Build Status](https://travis-ci.org/r-lib/httrmock.svg?branch=master)](https://travis-ci.org/r-lib/httrmock)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/httrmock?svg=true)](https://ci.appveyor.com/project/gaborcsardi/httrmock)
[![](http://www.r-pkg.org/badges/version/httrmock)](http://www.r-pkg.org/pkg/httrmock)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/httrmock)](http://www.r-pkg.org/pkg/httrmock)


Mock HTTP Requests for API Testing

## Installation


```r
devtools::install_github("r-lib/httrmock")
```

## Introduction

TODO

`httrmock` was inspired by https://github.com/assaf/node-replay

## Usage in packages:

TODO

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

MIT © Gábor Csárdi, RStudio
