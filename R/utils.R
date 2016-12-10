
drop_nulls <- function(x) {
  x[! vapply(x, is.null, logical(1))]
}
