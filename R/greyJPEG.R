#' Greyscale conversion
#'
#' Open a jpg file and convert it to greyscale.
#' @param filepath a charcter string specifying the location of the video file
#' @return A matrix of pixel values
#' @importFrom jpeg readJPEG
#' @export
greyJPEG = function(filepath) {
  jpeg = readJPEG(filepath)
  grey.jpeg = (jpeg[,,1]*0.2126)+(jpeg[,,2]*0.7152)+(jpeg[,,3]*0.0722)
  return(grey.jpeg)
}
