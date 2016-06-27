#' Track an animal's movement across a series of still frames
#'
#' This function is the core of the \code{pathtrackr} package. It takes a series of jpegs (extracted from a video) as inputs and tracks an animal's movement across frames. A list is returned containing the xy co-ordinates of the animal in each frame, as well as summary statistics. The returned list can be called for plotting and further functions in the \code{pathtrackr} package.
#' @param dirpath a character string specifying a directory containing only jpeg files extracted from a video
#' @param xarena an integer specifying the arena width in mm; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param yarena an integer specifying the arena height in mm; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param fps an integer specifying the frame rate at which jpegs were extracted from a video; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param box an integer specifying the size of the tracking box relative to the initial animal selection box; a larger box size will help prevent the animal being lost (useful for fast moving animals) but will also increase sensitivity to background and lighting changes (default 2)
#' @param contrast a value between 0 and 1 specifying the contrast threshold for distinguishing between the animal and background; by default \code{contrast} is set to 0.5, which will suit most situations, but may need adjusting for very low contrast videos
#' @details \code{trackPath} tracks an individual animal's movement across a series of still frames. The function utilises a focusing box to limit the search area for the animal relative to its previous position. This makes \code{trackPath} relatively robust to background lighting changes, extraneous backgroud movement and jpeg noise. It can also handle a dark animal on a light background and vice versa, but will not perform well if the contrast within the background is greater than the contrast between the animal and background.
#' @return A list containing a matrix of xy co-ordinates of the animal in each frame, a matrix of movement data including the distance, velocity and trajectories of movement between frames, and summary statistics.
#' @importFrom jpeg readJPEG
#' @importFrom raster raster extent select
#' @importFrom SDMTools COGravity
#' @export
trackPath = function(dirpath, xarena, yarena, fps, box = 2, contrast = 0.5) {

  xpos = c()
  ypos = c()
  breaks = c()
  break.count = 1
  animal.size = c()
  animal.last = c()

  file.list = list.files(dirpath, full.names = T)
  frame.calib = jpeg::readJPEG(file.list[1])
  plot(raster::raster(file.list[1], band = 2), col = gray.colors(256))

  message("Select a portion of the image that includes the entire animal...")
  flush.console()
  animal.crop = as.vector(raster::extent(raster::select(raster::raster(file.list[1], band = 2))))
  animal.frame = frame.calib[(nrow(frame.calib) - animal.crop[3]):(nrow(frame.calib) - animal.crop[4]), animal.crop[1]:animal.crop[2], 1:3]
  animal.gray = (animal.frame[,,1] * 0.2126) + (animal.frame[,,2] * 0.7152) + (animal.frame[,,3] * 0.0722)
  animal.mean = mean(animal.gray)

  bg.edge1 = animal.gray[1,1:ncol(animal.gray)]
  bg.edge2 = animal.gray[nrow(animal.gray),1:ncol(animal.gray)]
  bg.edge3 = animal.gray[1:nrow(animal.gray),1]
  bg.edge4 = animal.gray[1:nrow(animal.gray), ncol(animal.gray)]
  bg.mean = mean(c(bg.edge1, bg.edge2, bg.edge3, bg.edge4))

  thresh = animal.mean - bg.mean

  message("Define the opposing corners of the entire arena...")
  flush.console()
  bg.crop = as.vector(raster::extent(raster::select(raster::raster(file.list[1], band = 2))))
  frame.bg = frame.calib[(nrow(frame.calib) - bg.crop[3]):(nrow(frame.calib) - bg.crop[4]), bg.crop[1]:bg.crop[2], 1:3]
  xpix = ncol(frame.bg)
  ypix = nrow(frame.bg)

  ref.x1 = animal.crop[1] - bg.crop[1]
  ref.x2 = ref.x1 + (animal.crop[2] - animal.crop[1])
  ref.y1 = animal.crop[3] - bg.crop[3]
  ref.y2 = ref.y1 + (animal.crop[4] - animal.crop[3])

  frame.ref = frame.bg
  frame.ref = (frame.ref[,,1] * 0.2126) + (frame.ref[,,2] * 0.7152) + (frame.ref[,,3] * 0.0722)
  frame.ref[ref.y1:ref.y2, ref.x1:ref.x2] = bg.mean

  pb = txtProgressBar(min = 0, max = length(file.list), initial = 0, style = 3)

  for (i in 1:length(file.list)) {

    frame.new = jpeg::readJPEG(file.list[i])
    frame.new = frame.new[(nrow(frame.new) - bg.crop[3]):(nrow(frame.new) - bg.crop[4]), bg.crop[1]:bg.crop[2], 1:3]
    frame.new = (frame.new[,,1] * 0.2126) + (frame.new[,,2] * 0.7152) + (frame.new[,,3] * 0.0722)
    frame.diff = frame.new - frame.ref

    if (i == 1) {
      track.box = frame.diff[ref.y2:ref.y1, ref.x1:ref.x2]
      track.box = (track.box - min(track.box))/(max(track.box) - min(track.box))

      if (thresh < 0) {
        frame.shift = matrix(1, nrow(frame.diff), ncol(frame.diff))
        frame.shift[ref.y2:ref.y1, ref.x1:ref.x2] = track.box
        animal.x = which(frame.shift < contrast, arr.ind = T)[,2]
        animal.y = (1 - (which(frame.shift < contrast, arr.ind = T)[,1])/nrow(frame.shift)) * nrow(frame.shift)
        z = rep(1, length = length(which(frame.shift < contrast, arr.ind = T)[,1]))
        wt = rep(1, length = length(which(frame.shift < contrast, arr.ind = T)[,1]))
        COG1 = SDMTools::COGravity(animal.x, animal.y, z, wt)
        animal.last = which(frame.shift < contrast)

      } else {
        frame.shift = matrix(0, nrow(frame.diff), ncol(frame.diff))
        frame.shift[ref.y2:ref.y1, ref.x1:ref.x2] = track.box
        animal.x = which(frame.shift > contrast, arr.ind = T)[,2]
        animal.y = (1 - (which(frame.shift > contrast, arr.ind = T)[,1])/nrow(frame.shift)) * nrow(frame.shift)
        z = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        wt = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        COG1 = SDMTools::COGravity(animal.x, animal.y, z, wt)
      }
      xpos[i] = COG1[1]
      ypos[i] = nrow(frame.diff) - COG1[3]
      animal.size[i] = length(animal.x)

    } else {
      x1 = xpos[i - 1] - (abs(ref.x1 - ref.x2)) * box
      if (x1 < 0) {x1 = 0}
      if (x1 > ncol(frame.diff)) {x1 = ncol(frame.diff)}
      x2 = xpos[i - 1] + (abs(ref.x1 - ref.x2)) * box
      if (x2 < 0) {x2 = 0}
      if (x2 > ncol(frame.diff)) {x2 = ncol(frame.diff)}
      y1 = ypos[i - 1] - (abs(ref.y1 - ref.y2)) * box
      if (y1 < 0) {y1 = 0}
      if (y1 > nrow(frame.diff)) {y1 = nrow(frame.diff)}
      y2 = ypos[i - 1] + (abs(ref.y1 - ref.y2)) * box
      if (y2 < 0) {y2 = 0}
      if (y2 > nrow(frame.diff)) {y2 = nrow(frame.diff)}
      track.box = frame.diff[c(y2:y1), c(x1:x2)]
      track.box = (track.box - min(track.box))/(max(track.box) - min(track.box))

      if (thresh < 0) {
        frame.shift = matrix(1, nrow(frame.diff), ncol(frame.diff))
        frame.shift[y2:y1, x1:x2] = track.box
        animal.x = which(frame.shift < contrast, arr.ind = T)[,2]
        animal.y = (1 - (which(frame.shift < contrast, arr.ind = T)[,1])/nrow(frame.shift)) * nrow(frame.shift)
        z = rep(1, length = length(which(frame.shift < contrast, arr.ind = T)[,1]))
        wt = rep(1, length = length(which(frame.shift < contrast, arr.ind = T)[,1]))
        COG1 = SDMTools::COGravity(animal.x, animal.y, z, wt)

      } else {
        frame.shift = matrix(0, nrow(frame.diff), ncol(frame.diff))
        frame.shift[y2:y1, x1:x2] = track.box
        animal.x = which(frame.shift > contrast, arr.ind = T)[,2]
        animal.y = (1 - (which(frame.shift > contrast, arr.ind = T)[,1])/nrow(frame.shift)) * nrow(frame.shift)
        z = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        wt = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        COG1 = SDMTools::COGravity(animal.x, animal.y, z, wt)
      }
      animal.new = which(frame.shift < contrast)
      animal.move = (length(na.omit(match(animal.last, animal.new))))/(max(c(length(animal.last), length(animal.new))))
      animal.last = animal.new

      if (length(animal.x) <= mean(animal.size) * 2) {
        if (animal.move < 0.9) {
          xpos[i] = COG1[1]
          ypos[i] = nrow(frame.diff) - COG1[3]

        } else {
          xpos[i] = xpos[i - 1]
          ypos[i] = ypos[i - 1]
        }
        animal.size[i] = length(animal.x)

      } else {
        if (thresh < 0) {
          frame.break = ((frame.diff - min(frame.diff))/(max(frame.diff) - min(frame.diff)))
          frame.break.animal = length(which(frame.break < contrast))

          if (frame.break.animal <= mean(animal.size) * 2) {
            animal.x = which(frame.break < contrast, arr.ind = T)[,2]
            animal.y = which(frame.break < contrast, arr.ind = T)[,1]
            z = rep(1, length = length(which(frame.break < contrast, arr.ind = T)[,1]))
            wt = rep(1, length = length(which(frame.break < contrast, arr.ind = T)[,1]))
            COG2 = SDMTools::COGravity(animal.x, animal.y, z, wt)

            xpos[i] = COG2[1]
            ypos[i] = COG2[3]

          } else {
            xpos[i] = xpos[i - 1]
            ypos[i] = ypos[i - 1]

          }
          animal.size[i] = animal.size[i - 1]
          breaks[break.count] = i
          break.count = break.count + 1

        } else {
          frame.break = ((frame.diff - min(frame.diff))/(max(frame.diff) - min(frame.diff)))
          frame.break.animal = length(which(frame.break > contrast))

          if (frame.break.animal <= mean(animal.size) * 2) {
            animal.x = which(frame.break > contrast, arr.ind = T)[,2]
            animal.y = which(frame.break > contrast, arr.ind = T)[,1]
            z = rep(1, length = length(which(frame.break > contrast, arr.ind = T)[,1]))
            wt = rep(1, length = length(which(frame.break > contrast, arr.ind = T)[,1]))
            COG2 = SDMTools::COGravity(animal.x, animal.y, z, wt)
            xpos[i] = COG2[1]
            ypos[i] = COG2[3]

          } else {
            xpos[i] = xpos[i - 1]
            ypos[i] = ypos[i - 1]

          }
          animal.size[i] = animal.size[i - 1]
          breaks[break.count] = i
          break.count = break.count + 1
        }
      }
    }

    setTxtProgressBar(pb, i)

  }
  ypos = nrow(frame.ref) - ypos

  time = seq(0, length.out = length(xpos), by = 1/fps)
  distance = c()
  abs.angle = c()
  rel.angle = c()
  velocity = c()
  count = 1
  for (j in 2:length(xpos)) {
    A = (xpos[j] - xpos[j - 1]) * (xarena/xpix)
    B = (ypos[j] - ypos[j - 1]) * (yarena/ypix)
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
  total.distance = round(sum(movement[,1]))
  mean.velocity = mean(movement[,4])
  total.duration = movement[nrow(movement),5]

  if (length(breaks) > 0) {
    warning("Tracking failed in a total of ", length(breaks), " frames: consider using a higher frame rate or increasing 'box'")
    flush.console()
  }

  return(list(position = cbind(xpos, ypos), dim.pix = c(xpix, ypix), dim.arena = c(xarena, yarena), fps = fps, movement = movement, total.distance = total.distance, mean.velocity = mean.velocity, total.duration = total.duration, breaks = breaks))

}
