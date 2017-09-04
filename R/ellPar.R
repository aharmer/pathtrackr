#' Calculate ellipse parameters
#'
#' Calculate ellipse parameters from blobs in binary matrix files.
#' @param mat a matrix of blobs (generated with a blob detection function)
#' @return A list of ellipse parameters calculated for the blob in the matrix.
#' @importFrom cluster ellipsoidhull
#' @noRd

# Calculate parameters of ellipse
ellPar = function(mat){
  ell = ellipsoidhull(mat)
  exy = predict(ell)
  centre = ell$loc
  dist2center = sqrt(rowSums((t(t(exy)-centre))^2))
  major = 2*max(dist2center)
  minor = 2*min(dist2center)
  area = pi*(major/2)*(minor/2)
  eigVec = eigen(ell$cov)$vectors
  vertices = rbind(c((ell$loc + major/2 * eigVec[,1])[1], (ell$loc + major/2 * eigVec[,1])[2]), c((ell$loc - major/2 * eigVec[,1])[1], (ell$loc - major/2 * eigVec[,1])[2]))
  bearing = unname((atan2(vertices[2,1] - vertices[1,1], vertices[2,2] - vertices[1,2]) * (180/pi)) %% 360)

  return(list(boundary = exy, centre = centre, major.axis = major, minor.axis = minor, vertices = vertices, area = area, bearing = bearing))
}
