#' NA filler
#'
#' Replace NAs with latest non-NA value
#' @param x a vector or matrix with NA values
#' @return A vector or matrix with NAs filled with last non-NA value
#' @noRd
na.lomf = function(x) {

  na.lomf.0 = function(x) {
    non.na.idx = which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx = c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }

  dim.len = length(dim(x))

  if (dim.len == 0L) {
    na.lomf.0(x)
  } else {
    apply(x, dim.len, na.lomf.0)
  }
}
