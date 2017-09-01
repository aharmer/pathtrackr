#' Matrix reversal by row
#'
#' Reverse row order of matrix for correctly oriented raster plots.
#' @param mat a matrix to be reflected around horizontal axis
#' @return A matrix
#' @export
reflect = function(mat) {
  mat[nrow(mat):1,]
}
