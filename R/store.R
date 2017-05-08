
#' @importFrom storr storr_rds
#' @importFrom debugme debugme

get_storr <- function() {
  storrpath <- Sys.getenv("HTTRMOCK_STORE", "")
  if (storrpath == "") {
    storrpath <- file.path(get_test_root(), "httrmock")
  }
  "!DEBUG Getting HTTP data from '`storrpath`'"
  storr_rds(storrpath)
}
