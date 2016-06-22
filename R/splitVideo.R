#' Compress and split a video into jpeg files
#'
#' \code{splitVideo} uses FFmpeg via a system call to compress a video file and split it into separate jpeg files in a new directory.
#' @param filepath a charcter string specifying the location of the video file
#' @param fps an integer: the number of frames per second of video to keep; if \code{fps} is the as the frame rate of the video, all frames will be saved
#' @param xpix an integer: the width of saved jpegs in pixels; the default is 320
#' @param ypix an integer: the height of saved jpegs in pixels; the default is -1, which will preserve the aspect ratio
#' @details The tracking functions within the \code{pathtrackr} package do not require high resolution imagery. It is is recommened that videos are compressed using the \code{xpix} and \code{ypix} variables to improve processing time. If FFmpeg is not available, video frames can be extracted using other video editing software.
#' @return Returns a new directory (in the same directory as the video file) containing each video frame as a jpeg file.
#' @note \code{splitVideo} requires FFmpeg to be installed on your machine. FFmpeg is a cross-platform, open-source video editing tool. It can be downloaded from \url{https://ffmpeg.org}.
#' @export
splitVideo = function(filepath, fps, xpix = 320, ypix = -1) {

  system(paste("ffmpeg -loglevel panic -y -i ", filepath, " -vf scale=", xpix, ":", ypix, " ", gsub("\\.", "_COMPRESSED.", filepath), sep = ""))

  dir.create(file.path(unlist(strsplit(filepath, "\\."))[1]), showWarnings = FALSE)

  system(paste("ffmpeg -loglevel panic -y -i ", gsub("\\.", "_COMPRESSED.", filepath), " -q:v 2 -vf fps=", fps, " -b:v 2000 -bt 20M ", unlist(strsplit(filepath, "\\."))[1], "/frame%06d.jpg", sep = ""))
}
