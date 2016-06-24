#' Make a video of an animal's tracked path
#'
#' \code{makeVideo} uses a \code{trackPath} and FFmpeg to produce an mp4 video of an animal's movement along with tracking behaviour and summary plots.
#' @inheritParams trackPath
#' @details See documentation for \code{\link{trackPath}}.
#' @return An mp4 video of an animal's movement containing the original video, tracking behaviour plots and summary plots.
#' @note \code{makeVideo} requires FFmpeg to be installed on your machine. FFmpeg is a cross-platform, open-source video editing tool. It can be downloaded from \url{https://ffmpeg.org}.
#' @importFrom jpeg readJPEG
#' @importFrom raster raster extent select
#' @importFrom SDMTools COGravity
#' @export
makeVideo = function(dirpath, xarena, yarena, fps, box = 2, contrast = 0.5) {

  xpos = c()
  ypos = c()
  temp.movement = c()
  breaks = c()
  break.count = 1
  animal.size = c()
  animal.last = c()

  file.list = list.files(dirpath, full.names = T)
  frame.calib = readJPEG(file.list[1])
  plot(raster(file.list[1], band = 2), col = gray.colors(256))

  message("Select a portion of the image that includes the entire animal...")
  flush.console()
  animal.crop = as.vector(extent(select(raster(file.list[1], band = 2))))
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
  bg.crop = as.vector(extent(select(raster(file.list[1], band = 2))))
  # frame.bg = readJPEG(file.list[1])
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

  dir.create(paste(dirpath, "_temp", sep = ""))

  for (i in 1:length(file.list)) {

    fig = paste(dirpath, "_temp/plot_", unlist(strsplit(file.list[i], "/"))[length(unlist(strsplit(file.list[i], "/")))], sep = "")
    jpeg(fig, width = 1080 * 1.5, height = 1080)

    frame.new = readJPEG(file.list[i])
    frame.new = frame.new[(nrow(frame.new) - bg.crop[3]):(nrow(frame.new) - bg.crop[4]), bg.crop[1]:bg.crop[2], 1:3]
    frame.jpg = frame.new
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
        COG1 = COGravity(animal.x, animal.y, z, wt)
        animal.last = which(frame.shift < contrast)

      } else {
        frame.shift = matrix(0, nrow(frame.diff), ncol(frame.diff))
        frame.shift[ref.y2:ref.y1, ref.x1:ref.x2] = track.box
        animal.x = which(frame.shift > contrast, arr.ind = T)[,2]
        animal.y = (1 - (which(frame.shift > contrast, arr.ind = T)[,1])/nrow(frame.shift)) * nrow(frame.shift)
        z = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        wt = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        COG1 = COGravity(animal.x, animal.y, z, wt)
      }
      xpos[i] = COG1[1]
      ypos[i] = nrow(frame.diff) - COG1[3]
      animal.size[i] = length(animal.x)

      par(mfrow = c(2, 3), mar = c(5, 5, 2, 3) + 0.1, cex.axis = 1.5, cex.lab = 1.5)

      res = dim(frame.jpg)[1:2]
      plot(1, 1, xlim = c(1, res[1]), ylim = c(1, res[2]), type = "n", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
      rasterImage(raster::as.raster(frame.jpg[nrow(frame.jpg):1, , ]), 1, 1, res[1], res[2])

      plot(raster(frame.new[nrow(frame.new):1, ]), legend = FALSE, xaxs = "i", yaxs = "i", cex = 1.5, col = magma(256))
      rect(ref.x1/ncol(frame.diff), ref.y1/nrow(frame.diff), ref.x2/ncol(frame.diff), ref.y2/nrow(frame.diff), border = "yellow", lwd = 1.5)
      points(xpos[i]/ncol(frame.new), ypos[i]/nrow(frame.new), col = "green", pch = 16)

      plot(raster(track.box), legend = FALSE, xaxs = "i", yaxs = "i", cex = 1.5, col = magma(256))
      points((xpos[i] - ref.x1)/ncol(track.box), (ypos[i] - ref.y1)/nrow(track.box), col = "green", pch = 16, cex = 2.5)

      plot(xpos * (xarena/xpix), max(ypos * (yarena/ypix)) - ypos * (yarena/ypix), col = "#08306B", type = "l", lwd = 2, pch = 16, xlim = c(0, dim(frame.new)[2] * (xarena/xpix)), ylim = c(0, dim(frame.new)[1] * (yarena/ypix)), xlab = "Distance (mm)", ylab = "Distance (mm)", xaxs = "i", yaxs = "i", cex = 1.5)

      plot(temp.movement[, 3], cumsum(temp.movement[, 1]), type = "l", lwd = 2, xlab = "Time (s)", ylab = "Distance (mm)", bty = "l", xlim = c(0, length(file.list) * (1/fps)), ylim = c(0, 0.1), col = "#08306B", cex = 1.5)

      plot(temp.movement[, 3], temp.movement[, 2], type = "l", lwd = 1.5, xlab = "Time (s)", ylab = "Velocity (mm/s)", bty = "l", xlim = c(0, length(file.list) * (1/fps)), ylim = c(0, 0.1), col = "#08306B", cex = 1.5)

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
        COG1 = COGravity(animal.x, animal.y, z, wt)

      } else {
        frame.shift = matrix(0, nrow(frame.diff), ncol(frame.diff))
        frame.shift[y2:y1, x1:x2] = track.box
        animal.x = which(frame.shift > contrast, arr.ind = T)[,2]
        animal.y = (1 - (which(frame.shift > contrast, arr.ind = T)[,1])/nrow(frame.shift)) * nrow(frame.shift)
        z = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        wt = rep(1, length = length(which(frame.shift > contrast, arr.ind = T)[,1]))
        COG1 = COGravity(animal.x, animal.y, z, wt)
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
            COG2 = COGravity(animal.x, animal.y, z, wt)

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
            COG2 = COGravity(animal.x, animal.y, z, wt)
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

      temp.time = seq(0, length.out = length(xpos), by = 1/fps)
      temp.distance = c()
      temp.velocity = c()
      temp.count = 1
      for (l in 2:length(xpos)) {
        temp.A = abs(xpos[l] - xpos[l - 1]) * (xarena/xpix)
        temp.B = abs(ypos[l] - ypos[l - 1]) * (yarena/ypix)
        temp.distance[temp.count] = sqrt((temp.A^2) + (temp.B^2))
        temp.velocity[temp.count] = temp.distance[temp.count]/(1/fps)
        temp.count = temp.count + 1
      }
      temp.movement = matrix(ncol = 3, nrow = temp.count)
      colnames(temp.movement) = c("distance", "velocity", "time")
      temp.movement[, 1] = c(0, temp.distance)
      temp.movement[, 2] = c(0, temp.velocity)
      temp.movement[, 3] = c(temp.time)

      par(mfrow = c(2, 3), mar = c(5, 5, 2, 3) + 0.1, cex.axis = 1.5, cex.lab = 1.5)

      res = dim(frame.jpg)[1:2]
      plot(1, 1, xlim = c(1, res[1]), ylim = c(1, res[2]), type = "n", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
      rasterImage(raster::as.raster(frame.jpg[nrow(frame.jpg):1, , ]), 1, 1, res[1], res[2])

      plot(raster(frame.new[nrow(frame.new):1, ]), legend = FALSE, xaxs = "i", yaxs = "i", cex = 1.5, col = magma(256))
      rect(x1/ncol(frame.diff), y1/nrow(frame.diff), x2/ncol(frame.diff), y2/nrow(frame.diff), border = "yellow", lwd = 1.5)
      points(xpos[i]/ncol(frame.new), ypos[i]/nrow(frame.new), col = "green", pch = 16)
      segments(xpos[i - 1]/ncol(frame.new), ypos[i - 1]/nrow(frame.new), xpos[i]/ncol(frame.new), ypos[i]/nrow(frame.new), col = "green", pch = 16)

      plot(raster(track.box), legend = FALSE, xaxs = "i", yaxs = "i", cex = 1.5, col = magma(256))
      points((xpos[i] - x1)/ncol(track.box), (ypos[i] - y1)/nrow(track.box), col = "green", pch = 16, cex = 2.5)
      segments((xpos[i - 1] - x1)/ncol(track.box), (ypos[i - 1] - y1)/nrow(track.box), (xpos[i] - x1)/ncol(track.box), (ypos[i] - y1)/nrow(track.box), col = "green", pch = 16, lwd = 3)

      plot(xpos * (xarena/xpix), ypos * (yarena/ypix), col = "#08306B", type = "l", lwd = 2, pch = 16, xlim = c(0, dim(frame.new)[2] * (xarena/xpix)), ylim = c(0, dim(frame.new)[1] * (yarena/ypix)), xlab = "Distance (mm)", ylab = "Distance (mm)", xaxs = "i", yaxs = "i", cex = 1.5)

      plot(temp.movement[, 3], cumsum(temp.movement[, 1]), type = "l", lwd = 2, xlab = "Time (s)", ylab = "Distance (mm)", bty = "l", xlim = c(0, length(file.list) * (1/fps)), col = "#08306B", cex = 1.5)

      plot(temp.movement[, 3], temp.movement[, 2], type = "l", lwd = 1.5, xlab = "Time (s)", ylab = "Velocity (mm/s)", bty = "l", xlim = c(0, length(file.list) * (1/fps)), col = "#08306B", cex = 1.5)

    }

    dev.off()
    setTxtProgressBar(pb, i)

  }
  ypos = nrow(frame.ref) - ypos

  system(paste("ffmpeg -loglevel panic -y -framerate ", fps, " -pattern_type glob -i ", paste(dirpath, "_temp/'*.jpg'", sep = ""), " -c:v libx264 -r 25 -pix_fmt yuv420p ", paste(paste(unlist(strsplit(dirpath, "/"))[1:(length(unlist(strsplit(dirpath, "/"))) - 1)], collapse = "/"), "/", unlist(strsplit(dirpath, "/"))[length(unlist(strsplit(dirpath, "/")))], sep = ""), "_TRACKED.mp4", sep = ""))

  unlink(paste(dirpath, "_temp", sep = ""), recursive = TRUE)

  if (length(breaks) > 0) {
    warning("Tracking failed in a total of ", length(breaks), " frames: consider using a higher frame rate or increasing 'box'")
    flush.console()
  }

}
