#' Manually track an animal's movement across a series of still frames
#'
#' This function is similar to the core \code{trackPath} of the \code{pathtrackr} package. However, rather than automatically tracking the animal, it requires the user to manual click on the position of the animal in each frame. This function is useful when your video is not appropriate for automatic video tracking, such as very low contrast videos, or videos with a lot of background movement. The same output as \code{trackPath} will be produced, i.e. a list containing the xy co-ordinates of the animal in each frame, as well as summary statistics. The returned list can be called for plotting and further functions in the \code{pathtrackr} package.
#' @param dirpath a character string specifying a directory containing only jpeg files extracted from a video
#' @param xarena an integer specifying the arena width in mm; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param yarena an integer specifying the arena height in mm; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param fps an integer specifying the frame rate at which jpegs were extracted from a video; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @details \code{manualPath} tracks an individual animal's movement across a series of still frames by requiring the user to manually click onht eanimal's position in each frame.
#' @return A list containing a matrix of xy co-ordinates of the animal in each frame, a matrix of movement data including the distance, velocity and trajectories of movement between frames, and summary statistics.
#' @importFrom raster raster
#' @export
manualPath = function(dirpath, xarena, yarena, fps = 30) {

  file.list = list.files(dirpath, full.names = TRUE)

  bg.dim = dim(greyJPEG(file.list[1]))
  xpos = c()
  ypos = c()

  for (i in 1:length(file.list)) {
    par(mar = c(1,0,0,0))
    plot(raster(file.list[i], band = 2), col = gray.colors(256), asp = 1, legend = FALSE)
    mtext("x", side = 1, line = 0, adj = 0.0, cex = 2, col = "red", outer = TRUE)
    mtext("Click the 'x' if the animal is not visible", side = 1, line = 1, adj = 0.0, cex = 1, col = "red", outer = TRUE)
    temp = locator(1)
    xpos[i] = round(temp[[1]])
    ypos[i] = round(temp[[2]])
  }

  xpos = ifelse(xpos < 0, NA, xpos)
  ypos = ifelse(ypos < 0, NA, ypos)
  breaks = which(is.na(xpos))

  time = seq(0, length.out = length(xpos), by = 1/fps)
  distance = c()
  abs.angle = c()
  rel.angle = c()
  velocity = c()
  count = 1
  for (j in 2:length(xpos)) {
    A = (xpos[j] - xpos[j - 1]) * (xarena/bg.dim[2])
    B = (ypos[j] - ypos[j - 1]) * (yarena/bg.dim[1])
    distance[count] = sqrt((A^2) + (B^2))
    abs.angle[count] = ifelse(distance[count] != 0 | count == 1, (atan2(A, B * -1) * (180/pi)) %% 360, abs.angle[count - 1])
    rel.angle[count] = ((((abs.angle[count] - abs.angle[count - 1]) %% 360) + 540) %% 360) - 180
    velocity[count] = distance[count]/(1/fps)
    count = count + 1
  }

  movement = matrix(ncol = 5, nrow = count)
  colnames(movement) = c("distance", "abs.angle", "rel.angle", "velocity", "time")
  movement[, 1] = c(0, distance)
  movement[, 2] = c(0, abs.angle)
  movement[, 3] = c(0, abs.angle[1], rel.angle[2:length(rel.angle)])
  movement[, 4] = c(0, velocity)
  movement[, 5] = c(time)
  total.distance = round(sum(movement[,1], na.rm = TRUE))
  mean.velocity = mean(movement[,4], na.rm = TRUE)
  total.duration = movement[nrow(movement),5]

  return(list(position = cbind(xpos, ypos), dim.pix = c(bg.dim[2], bg.dim[1]), dim.arena = c(xarena, yarena), fps = fps, movement = movement, total.distance = total.distance, mean.velocity = mean.velocity, total.duration = total.duration, breaks = breaks))

}
