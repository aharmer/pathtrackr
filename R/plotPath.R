#' Plot animal movement data
#'
#' This function plots the path of an animal tracked using the \code{trackPath} function.
#' @param path.list a list object created by the \code{trackPath} function
#' @details This function relies on the ggplot package.
#' @return A density plot of time spent in a position overlaid with the tracked animal's path.
#' @import ggplot2
#' @export
plotPath = function(path.list) {

dat = as.data.frame(path.list$position)
dat$xpos = dat$xpos * (path.list$dim.arena[1]/path.list$dim.pix[1])
dat$ypos = dat$ypos * (path.list$dim.arena[2]/path.list$dim.pix[2])
x_max = path.list$dim.pix[2] * (path.list$dim.arena[2]/path.list$dim.pix[2])
y_max = path.list$dim.pix[1] * (path.list$dim.arena[1]/path.list$dim.pix[1])

ggplot(aes(xpos, ypos), data = dat) +
  stat_density2d(aes(fill = ..density.., alpha = ..density..), geom = "tile", contour = FALSE) +
  scale_fill_gradientn(colours = viridis::viridis(256)) +
  geom_path(na.rm = TRUE) +
  geom_point(aes(x = dat[1,1], y = dat[1,2]), size = 3, color = "blue") +
  geom_point(aes(x = dat[nrow(dat),1], y = dat[nrow(dat),2]), size = 3, color = "red") +
  scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
  xlab("Distance (mm)") +
  ylab("Distance (mm)") +
  theme_bw() +
  theme(legend.position = "none", axis.title = element_text(size = 14, face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

}
