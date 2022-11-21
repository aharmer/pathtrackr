#' Approxiamte diagonal calculation (from 'marmap' package)
#'
#' Used in countCells function.
#' @param mat a matrix
#' @return Diagonal distance.
#' @noRd

diag.bathy = function(mat, coord = FALSE){

  as.numeric(rownames(mat)) -> lon
  as.numeric(colnames(mat)) -> lat

  m = nrow(mat) # lon
  n = ncol(mat) # lat

  coord.m = round(seq(1, m, length.out = max(m, n)), 0)
  coord.n = round(seq(1, n, length.out = max(m, n)), 0)

  data.frame(lon[coord.m], lat[coord.n]) -> coord.tab
  names(coord.tab) <- c("lon", "lat")

  dia = NULL
  for(i in 1:max(m, n)){
    dia = c(dia, mat[coord.m[i], coord.n[i]])
  }

  if(coord == FALSE) return(dia) else return(coord.tab)

}
