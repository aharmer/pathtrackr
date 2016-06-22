#' Calculate a moving average
#'
#' Calculate a moving average and fill in missing values.
#' @param x a vector of numeric values
#' @param n the number of samples to average across
#' @param centered logical, if \code{FALSE}, then average current sample and previous (n-1) samples; if \code{TRUE}, then average symmetrically in past and future (if n is even, use one more sample from future)
#' @details This function is taken directly from the \href{http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/}{Cookbook for R} website.
#' @return A vector of values averaged across n samples
#' @noRd
movingAverage = function(x, n = 1, centered = FALSE) {

  if (centered) {
    before = floor((n - 1)/2)
    after = ceiling((n - 1)/2)
  } else {
    before = n - 1
    after = 0
  }
  s = rep(0, length(x))
  count = rep(0, length(x))
  new = x
  count = count + (!is.na(new))
  new[is.na(new)] = 0
  s = s + new
  i = 1
  while (i <= before) {
    new = c(rep(NA, i), x[1:(length(x) - i)])
    count = count + (!is.na(new))
    new[is.na(new)] = 0
    s = s + new
    i = i + 1
  }
  i = 1
  while (i <= after) {
    new = c(x[(i + 1):length(x)], rep(NA, i))
    count = count + (!is.na(new))
    new[is.na(new)] = 0
    s = s + new
    i = i + 1
  }
  s/count
}
