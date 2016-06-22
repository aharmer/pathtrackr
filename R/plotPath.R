#' Plot animal movement data
#'
#' This function plots the path of an animal tracked using the \code{trackPath} function.
#' @param path.list a list object created by the \code{trackPath} function
#' @details This functions relies on the \code{smoothScatter} function from the base \code{graphics} package.
#' @return A density plot of time spent in a position overlaid with the tracked animal's path.
#' @export
plotPath = function(path.list) {

  smoothScatter(path.list$position[,1] * (path.list$dim.arena[1]/path.list$dim.pix[1]), path.list$dim.arena[2] - path.list$position[,2] * (path.list$dim.arena[2]/path.list$dim.pix[2]), nbin = 250, nrpoints = 0, xlab = "Distance (mm)", ylab = "Distance (mm)", colramp = colorRampPalette(c("white", blues9)), xlim = c(0, path.list$dim.arena[1]), ylim = c(0, path.list$dim.arena[2]))

  points(path.list$position[,1] * (path.list$dim.arena[1]/path.list$dim.pix[1]), path.list$dim.arena[2] - path.list$position[,2] * (path.list$dim.arena[2]/path.list$dim.pix[2]), col = "#08306B", type = "l", lwd = 1.5)

  points(path.list$position[1,1] * (path.list$dim.arena[1]/path.list$dim.pix[1]), path.list$dim.arena[2] - path.list$position[1,2] * (path.list$dim.arena[2]/path.list$dim.pix[2]), col = "green", pch = 19)

  points(path.list$position[length(path.list$position[,1]),1] * (path.list$dim.arena[1]/path.list$dim.pix[1]), path.list$dim.arena[2] - path.list$position[length(path.list$position[,2]),2] * (path.list$dim.arena[2]/path.list$dim.pix[2]), col = "red", pch = 19)

}
