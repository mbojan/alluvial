#' Alluvial diagram for multiple time series data
#'
#' This is a variant of alluvial diagram suitable for multiple
#' (cross-sectional) time series. It also works with continuous variables equivalent to time
#' 
#' @param dat data.frame of time-series (or suitable equivalent continuously disaggregated data), with 3 columns (in order: category, time-variable, value) with <= 1 row for each category-time combination
#' @param wave numeric, curve wavyness defined in terms of x axis data range - i.e. bezier point offset. Experiment to get this right
#' @param ygap numeric, vertical distance between polygons - a multiple of 10\% of the mean data value
#' @param col colour, value or vector of length matching the number of unique categories. Individual colours of vector are mapped to categories in alpha-numeric order
#' @param lwd numeric, value or vector of length matching the number of unique categories for polygon stroke line width. Individual values of vector are mapped to categories in alpha-numeric order
#' @param alpha numeric, [0,1] polygon fill transparency
#' @param plotdir character, string ('up', 'down' or 'centred') giving the vertical alignment of polygon stacks
#' @param rankup logical, rank polygons on time axes upward by magnitude (largest to smallest) or not
#' @param lab.cex numeric, category label font size
#' @param lab.col colour, of category label
#' @param xmargin numeric [0,1], proportional space for category labels
#' @param axis.col colour, of axes
#' @param axis.cex numeric, font size of x-axis break labels
#' @param title character, plot title
#' @param title.cex numeric, plot title font size
#' @param grid logical, plot vertical axes
#' @param grid.col colour, of grid axes
#' @param grid.lwd numeric, line width of grid axes
#' @param leg.mode logical, draw y-axis scale legend inside largest data point (TRUE default) or alternatively with custom position/value (FALSE)
#' @param leg.x,leg.y numeric [0,1], x/y positions of legend if leg.mode = FALSE
#' @param leg.cex numeric, legend text size
#' @param leg.col colour, of legend lines and text
#' @param leg.lty numeric, code for legend line type
#' @param leg.lwd numeric, legend line width
#' @param leg.max numeric, legend scale line width
#' @param xlab,ylab character, x-axis / y-axis titles
#' @param xlab.pos,ylab.pos numeric, perpendicular offset for axis titles
#' @param ... arguments to pass to polygon()
#'
#' @export
#'
#' @example examples/alluvial_ts.R


alluvial_ts <- function(dat, wave = NA, ygap = 1, col = NA, alpha = NA, plotdir = 'up', rankup = FALSE, 
                        lab.cex = 1, lab.col = 'black', xmargin = .1, axis.col = 'black', title = NA, 
                        title.cex = 1, axis.cex = 1, grid = FALSE, grid.col = 'grey80', grid.lwd = 1, 
                        leg.mode = TRUE, leg.x = .1, leg.y = .9, leg.cex = 1, leg.col = 'black', leg.lty = NA, 
                        leg.lwd = NA, leg.max = NA, xlab = NA, ylab = NA, xlab.pos = 2, ylab.pos = 1, lwd = 1, ...){
  
  orig.names <- names(dat)
  names(dat) <- c('item', 'time', 'val')
  if(is.numeric(dat$item)) dat$item <- as.character(dat$item)
  if(is.ordered(dat$time) | is.factor(dat$time)) {
    axis.labs <- levels(dat$time)
    dat$time <- as.numeric(dat$time)
  } else if(is.numeric(dat$time)) axis.labs <- sort(unique(dat$time)) else {
    return("Error: time variable must be numeric, factor, or ordered")}
  times <- sort(unique(dat$time))
  
  dat <- dat[order(dat$item), ]
  datsum <- aggregate(val ~ item, dat, mean)
  plotorder <- order(datsum$val, decreasing = TRUE) # smallest last (on top)
  maxval <- pretty(max(dat$val))[2] # legend max
  
  # colours
  n <- length(unique(dat$item))
  if(all(is.na(col))) col <- rainbow(n)
  if(length(col) == 1) col <- rep(col, n)
  col <- rgb(t(col2rgb(col)), maxColorValue = 255) # ensure hex
  if(!length(col) == n) return("Error: 'col' length must equal the number of unique data elements")
  if(!is.na(alpha)) {
    col[nchar(col)>7] <- substr(col[nchar(col)>7], 1, 7)
    col <- paste0(col, substr(rgb(0, 0, alpha), 6, 7))
  }
  
  # calc vertical gap between items
  ymean <- mean(dat$val)
  ygap <- ymean * .1 * ygap
  
  # if not specified (but it really should be)
  if(is.na(wave)) wave <- .5 * (rev(times)[1] - times[1])/length(times)
  
  plot.y.max <- 0  # vertical plot scaling
  
  # prepare main data object
  d <- list()
  for(i in unique(dat$item)) d[[i]] <- list()
  
  # loop through periods
  for(i in 1:length(times)){
    
    # 3 time variables, NA if they fall outside the data period
    if(i>1) t1 <- times[i-1] else t1 <- NA              # prev period
    t2 <- times[i]                                     # this period
    if(i<length(times)) t3 <- times[i+1] else t3 <- NA  # next period
    
    dat.t <- dat[dat$time == t2, ]
    dat.t <- if(rankup) { dat.t[order(dat.t$val, decreasing = TRUE), ] 
      } else dat.t[order(dat.t$val, decreasing = FALSE), ]
    
    y.sum <- sum(dat.t$val) + (nrow(dat.t) * ygap)
    if(y.sum > plot.y.max) plot.y.max <- y.sum
    
    # loop through items
    if(plotdir == 'centred') y <- -y.sum/2 else y <- 0  # vertical scaler
    
    for(j in 1:nrow(dat.t)){
      # work up/down y axes to calculate spline positions
      y0 <- y + ygap
      y1 <- y0 + dat.t$val[j]
      y <- y1
      
      # calculate left and right x-axis splines
      if(!is.na(t1)) spline.x <- c(t2 - wave, t2) else spline.x <- numeric(0)
      if(!is.na(t3)) spline.x <- c(spline.x, t2, t2 + wave)
      
      # update d
      item <- as.character(dat.t$item[j])
      d[[item]]$x  <- c(d[[item]]$x,  spline.x)
      d[[item]]$y0 <- c(d[[item]]$y0, rep(y0, length(spline.x)))
      d[[item]]$y1 <- c(d[[item]]$y1, rep(y1, length(spline.x)))
    } # end items loop
  } # end period loop
  
  # function to ensure vertex arrays are same length, as xspline output can vary
  resize <- function(v, n = 500){ d<-data.frame(x = 1:length(v), y = v); 
                                  approx(d, xout = seq(1, length(v), length.out = n))$y}
  plot.new() # required by xspline
  
  # calculate spline curves
  for(i in names(d)){
    curves <- list()
    # iterate through bottom/top sets of bezier points, get curves and resize
    for(j in c('y0', 'y1')) curves[[j]] <- lapply(xspline(d[[i]]$x, d[[i]][[j]], shape = 1, draw = FALSE), resize)
    # stitch top and bottom polylines together clockwise into a polygon
    d[[i]]$poly <- data.frame(x = c(curves[[1]]$x, rev(curves[[2]]$x)), y = c(curves[[1]]$y, rev(curves[[2]]$y)))
  }
  
  # label y positions
  labs.l <- data.frame(t(sapply(1:length(d), FUN = function(i)t(data.frame(lab = names(d[i]), lefty = mean(c(d[[i]]$y0[1], d[[i]]$y1[1]))) ))), stringsAsFactors = FALSE)
  labs.r <- data.frame(t(sapply(1:length(d), FUN = function(i)t(data.frame(lab = names(d[i]), lefty = mean(c(rev(d[[i]]$y0)[1], rev(d[[i]]$y1)[1])))) )), stringsAsFactors = FALSE)
  names(labs.l) <- names(labs.r) <- c('item', 'y')
  labs.l$y <- as.numeric(as.character(labs.l$y)); labs.r$y <- as.numeric(as.character(labs.r$y)) # to numeric
  labs.l$col <- col[match(labs.l$item, datsum$item)]
  labs.r$col <- col[match(labs.r$item, datsum$item)]
  
  # line widths
  if(any(lwd == 0)) return("Error: 'lwd' must be greater than zero")
  if(length(lwd) == 1) lwd <- rep(lwd, length(d))
  if(!length(lwd) == length(d)) return("Error: 'lwd' length must equal the number of unique data elements")
  lwd <- lwd[plotorder]

  # scale and orientation of axes
  if(plotdir == 'up')       ylim <- c(0, plot.y.max) else {
    if(plotdir == 'down')   ylim <- c(plot.y.max, 0) else {
      if(plotdir == 'centred') ylim <- c(-plot.y.max/2, plot.y.max/2) else {
        return("Incorrect specification for plotdir: please select 'up', 'down' or 'centred'")
      }}}
  xran <- range(dat$time)
  xlim <- extendrange(r = xran, f = xmargin/2)
  
  # plot order (biggest polygons first)
  d <- d[plotorder]
  col <- col[plotorder]
  
  # axis labels
  if(is.na(xlab)) xlab <- orig.names[2] 
  if(is.na(ylab)) ylab <- orig.names[3]
  
  # plot
  plot.window(xlim = xlim, ylim = ylim)
  axis(1, times, cex.axis = axis.cex, labels = axis.labs, col = lab.col, col.axis = axis.col)
  if(is.na(title)) title <- paste('Alluvial plot of', orig.names[1], 'vs', orig.names[3], 'by', orig.names[2])
  title(title, cex.main = title.cex)
  mtext(xlab, side = 1, line = xlab.pos, col = lab.col); mtext(ylab, side = 2, line = ylab.pos, col = lab.col)
  if(grid) abline(v = times, col = grid.col, lwd = grid.lwd)
  for(i in 1:length(d)) polygon(d[[i]]$poly$x, d[[i]]$poly$y, col = col[i], lwd = lwd[i], ...)
  text(times[1], labs.l$y, labels = labs.l$item, col = labs.l$col, pos = 2, cex = lab.cex)
  text(times[length(times)], labs.r$y, labels = labs.r$item, adj = 0, col = labs.r$col, pos = 4, cex = lab.cex)
  
  # legend
  topval  <- max(dat$val)
  topitem <- as.character(dat$item[match(topval, dat$val)])
  toptime <- dat$time[match(topval, dat$val)]
  
  if(leg.mode){ # legend plotted on maximum data point
    if(is.na(leg.lty)) leg.lty <- "dotted"; if(is.na(leg.lwd)) leg.lwd <- 2
    val_ind <- match(toptime, d[[topitem]]$x)
    leg_y0 <- d[[topitem]]$y0[val_ind]
    leg_y1 <- d[[topitem]]$y1[val_ind]
    leg_ym <- mean(c(leg_y0, leg_y1))
    lines(rep(toptime, 2), c(leg_y0, leg_ym-(topval*.08)), lwd = leg.lwd, lend = 'butt', col = leg.col, lty = leg.lty)
    lines(rep(toptime, 2), c(leg_y1, leg_ym+(topval*.08)), lwd = leg.lwd, lend = 'butt', col = leg.col, lty = leg.lty)
    text(toptime, leg_ym, labels = formatC(topval, format = "d", big.mark = ','), pos = NULL, cex = leg.cex, col = leg.col)
  } else {         # legend plotted in custom position
    if(!is.na(leg.max)) maxval <- leg.max
    if(is.na(leg.lty)) leg.lty <- "solid"; if(is.na(leg.lwd)) leg.lwd <- 10
    leg_x <- (xlim[2]-xlim[1]) * leg.x + xlim[1]
    leg_y <- (ylim[2]-ylim[1]) * leg.y + ylim[1]
    diffs <- d[[topitem]]$y1 - d[[topitem]]$y0
    time <- d[[topitem]]$x[match(max(diffs), diffs)]
    lines(data.frame(x = c(leg_x, leg_x), y = c(leg_y, leg_y + maxval)), lwd = leg.lwd, lend = 'butt', col = leg.col, lty = leg.lty)
    max.lab <- formatC(maxval, format = "d", big.mark = ',')
    text(rep(leg_x, 2), c(leg_y, leg_y + maxval), labels = c('0', max.lab), cex = leg.cex, pos = 4, offset = 1, col = leg.col)
  }
}
