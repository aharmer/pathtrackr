#' Track an animal's movement across a series of still frames
#'
#' This function is the core of the \code{pathtrackr} package. It takes a series of jpegs (extracted from a video) as inputs and tracks an animal's movement across frames. A list is returned containing the xy co-ordinates of the animal in each frame, as well as summary statistics. The returned list can be called for plotting and further functions in the \code{pathtrackr} package.
#' @param dirpath a character string specifying a directory containing only jpeg files extracted from a video
#' @param xarena an integer specifying the arena width in mm; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param yarena an integer specifying the arena height in mm; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param fps an integer specifying the frame rate at which jpegs were extracted from a video; this value is used for distance and velocity calculations, an incorrect value will not cause an error but will result in inaccurate calculations
#' @param box an integer specifying the size of the tracking box relative to the initial animal selection box; a larger box size will help prevent the animal being lost (useful for fast moving animals) but will also increase sensitivity to background and lighting changes (default 1)
#' @param jitter.damp a value between 0.5 and 1. Reduces noise in the animal's track, useful if the animal's track is very jittery, which may overestiamte path length. A value of 1 indicates no jitter damping, 0.5 indicates extreme damping and is unlikely to be useful
#' @details \code{trackPath} tracks an individual animal's movement across a series of still frames. The function utilises a focusing box to limit the search area for the animal relative to its previous position. This makes \code{trackPath} relatively robust to background lighting changes, extraneous backgroud movement and jpeg noise. It can  handle a dark animal on a light background and vice versa, as well as heterogenous backgrounds where the animal is at times darker and lighter than the background.
#' @return A list containing a matrix of xy co-ordinates of the animal in each frame, a matrix of movement data including the distance, velocity and trajectories of movement between frames, and summary statistics.
#' @importFrom raster raster extent select
#' @importFrom pbapply pboptions pbapply pblapply
#' @importFrom abind abind
#' @importFrom EBImage bwlabel opening thresh rmObjects
#' @importFrom imager isoblur as.cimg
#' @importFrom plyr count aaply create_progress_bar progress_text
#' @export
trackPath = function(dirpath, xarena, yarena, fps = 30, box = 1, jitter.damp = 0.9) {

  require(raster, quietly = TRUE)

  if (length(dir(dirpath, "*.jpg")) > 0) {
    file.list = list.files(dirpath, full.names = TRUE)
  } else {
    stop("No files were found... check that the path to your directory is correct and that it contains only jpg files.")
  }

  # Set progress bar options
  pboptions(type = "txt", char = ":")
  pbapp = create_progress_bar(name = "text", style = 3, char = ":", width = 50)

  # Crop array to area of interest if needed
  message("Click once on the top left corner of your arena, followed by clicking once on the bottom right corner of your arena, to define the opposing corners of the entire arena...\n")
  flush.console()
  plot(raster(file.list[1], band = 2), col = gray.colors(256), asp = 1, legend = FALSE)
  bg.crop = base::as.vector(extent(select(raster(file.list[1], band = 2))))

  # Get aniaml tracking box in first frame
  bg.ref = greyJPEG(file.list[1])
  bg.ref = bg.ref[(dim(bg.ref)[1] - bg.crop[3]):(dim(bg.ref)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
  bg.dim = dim(bg.ref)
  message("Imagine the minimum sized rectangle that encompasses your whole animal. Click once to define the top left corner of this rectangle, followed by clicking once to define the bottom right corner of this rectangle...\n")
  flush.console()
  plot(raster(reflect(bg.ref), xmn = 0, xmx = bg.dim[2], ymn = 0, ymx = bg.dim[1]), col = gray.colors(256), asp = 1, legend = FALSE)
  animal.crop = round(base::as.vector(extent(select(raster(bg.ref, xmn = 0, xmx = bg.dim[1], ymn = 0, ymx = bg.dim[2])))))

  ref.x1 = animal.crop[1]
  ref.x2 = animal.crop[2]
  ref.y1 = animal.crop[4]
  ref.y2 = animal.crop[3]
  dim.x = abs(ref.x1 - ref.x2)
  dim.y = abs(ref.y1 - ref.y2)

  # Generate background reference frame
  message("Generating background reference frame...\n")
  flush.console()
  if (length(file.list) >= 1000) {
    idx = sample(file.list, 1000)
    bg.sample = abind(pblapply(idx, greyJPEG), along = 3)
    bg.sample = bg.sample[(dim(bg.sample)[1] - bg.crop[3]):(dim(bg.sample)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2],]
    bg.med = pbapply(bg.sample, 1:2, median)
  } else {
    bg.sample = abind(pblapply(file.list, greyJPEG), along = 3)
    bg.sample = bg.sample[(dim(bg.sample)[1] - bg.crop[3]):(dim(bg.sample)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2],]
    bg.med = pbapply(bg.sample, 1:2, median)
  }

  message("\nTracking animal...\n")
  flush.console()

  # Loop through frames fitting tracking box and extracting animal position etc.
  xpos = c()
  ypos = c()
  animal.size = c()
  breaks = c()
  break.count = 1
  animal.last = c()
  blur = 5
  min.animal = 0.25
  max.animal = 1.75

  pbloop = txtProgressBar(min = 0, max = length(file.list), style = 3, char = ":", width = 50)

  for (i in 1:length(file.list)) {

    # For first frame...
    if (i == 1) {

      # Find, segment and label blobs, then fit an ellipse
      frame = greyJPEG(file.list[i])
      frame = frame[(dim(frame)[1] - bg.crop[3]):(dim(frame)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
      frame = abs(frame - bg.med)
      tbox = reflect(frame[ref.y1:ref.y2,ref.x1:ref.x2])
      tbox.bin = as.matrix(bwlabel(opening(thresh(isoblur(as.cimg(tbox), blur)))))
      animal = ellPar(which(tbox.bin == 1, arr.ind = TRUE))
      animal.last = which(tbox.bin == 1)

      # Correct xy positions relative to entire frame and store
      xpos[i] = round(animal$centre[2] + ref.x1)
      ypos[i] = round(animal$centre[1] + ref.y2)

      # Store animal size
      animal.size[i] = animal$area

      # For the remaining frames...
    } else {

      # Calculate co-oordinates to redraw tracking box around last position
      if (!is.na(tail(xpos, 1))) {x1 = xpos[i - 1] - dim.x * box}
      if (x1 < 0) {x1 = 0}
      if (x1 > bg.dim[2]) {x1 = bg.dim[2]}
      if (!is.na(tail(xpos, 1))) {x2 = xpos[i - 1] + dim.x * box}
      if (x2 < 0) {x2 = 0}
      if (x2 > bg.dim[2]) {x2 = bg.dim[2]}
      if (!is.na(tail(ypos, 1))) {y1 = ypos[i - 1] - dim.y * box}
      if (y1 < 0) {y1 = 0}
      if (y1 > bg.dim[1]) {y1 = bg.dim[1]}
      if (!is.na(tail(ypos, 1))) {y2 = ypos[i - 1] + dim.y * box}
      if (y2 < 0) {y2 = 0}
      if (y2 > bg.dim[1]) {y2 = bg.dim[1]}

      # Find, segment and label blobs, then fit an ellipse
      frame = greyJPEG(file.list[i])
      frame = frame[(dim(frame)[1] - bg.crop[3]):(dim(frame)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
      frame = abs(frame - bg.med)
      tbox = reflect(frame[y2:y1,x1:x2])
      tbox.bin = as.matrix(bwlabel(opening(thresh(isoblur(as.cimg(tbox), blur)))))

      # Calculate proportion of overlapping pixels from between current & previous frame
      animal.new = which(tbox.bin == 1)
      animal.move = (length(na.omit(match(animal.last, animal.new))))/(max(c(length(animal.last), length(animal.new))))
      animal.last = animal.new

      # Check if animal is of ~right size
      if (length(which(tbox.bin == 1)) > mean(animal.size, na.rm = TRUE)*min.animal & length(which(tbox.bin == 1)) < mean(animal.size, na.rm = TRUE)*max.animal) {

        # Check animal has moved my more than 10% of size
        if (animal.move < jitter.damp) {

          animal = ellPar(which(tbox.bin == 1, arr.ind = TRUE))

          # Correct xy positions relative to entire frame and store
          xpos[i] = round(animal$centre[2] + x1)
          ypos[i] = round(animal$centre[1] + y1)

          # Store animal size
          animal.size[i] = animal$area

        } else {

          # Store last known position of animal and size
          xpos[i] = xpos[i - 1]
          ypos[i] = ypos[i - 1]
          animal.size[i] = animal.size[i - 1]
        }

      } else {

        frame.break = greyJPEG(file.list[i])
        frame.break = frame.break[(dim(frame.break)[1] - bg.crop[3]):(dim(frame.break)[1] - bg.crop[4]), bg.crop[1]:bg.crop[2]]
        frame.break = abs(frame.break - bg.med)
        frame.break.bin = as.matrix(bwlabel(opening(thresh(isoblur(as.cimg(frame.break), blur)))))
        blob.pixcount = as.matrix(count(frame.break.bin[frame.break.bin > 0]))

        if (nrow(blob.pixcount) > 1) {
          frame.break.bin = rmObjects(frame.break.bin, blob.pixcount[blob.pixcount[,2] < mean(animal.size, na.rm = TRUE)*min.animal | blob.pixcount[,2] > mean(animal.size, na.rm = TRUE)*max.animal,1])
        }

        if (length(which(frame.break.bin == 1)) > mean(animal.size, na.rm = TRUE)*min.animal & length(which(frame.break.bin == 1)) < mean(animal.size, na.rm = TRUE)*max.animal) {

          animal = ellPar(which(frame.break.bin == 1, arr.ind = TRUE))

          # Correct xy positions relative to entire frame and store
          xpos[i] = round(animal$centre[2])
          ypos[i] = bg.dim[1] - round(animal$centre[1])

          # Store animal size
          animal.size[i] = animal$area

        } else {

          # Mark position and size as unknown
          xpos[i] = NA
          ypos[i] = NA
          animal.size[i] = NA

          # Store breaks
          breaks[break.count] = i
          break.count = break.count + 1
        }
      }
    }
    setTxtProgressBar(pbloop, i)
  }

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
    abs.angle[count] = ifelse(distance[count] != 0 | count == 1, (atan2(A, B) * (180/pi)) %% 360, abs.angle[count - 1])
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

  if (length(breaks) > 0) {
    warning("Tracking was not possible for ", length(breaks), " frames: you can proceed with this tracked path but you might consider using a higher frame rate or increasing the tracking 'box' size to improve the result.")
    flush.console()
  }

  return(list(position = cbind(xpos, ypos), dim.pix = c(bg.dim[2], bg.dim[1]), dim.arena = c(xarena, yarena), fps = fps, movement = movement, total.distance = total.distance, mean.velocity = mean.velocity, total.duration = total.duration, breaks = breaks))

}
