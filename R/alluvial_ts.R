#' Alluvial diagram for multiple time series data
#'
#' This is a variant of alluvial diagram suitable for multiple
#' (cross-sectional) time series.
#'
#' @param dat       should be a 3 column data.frame with fields in order as per example below
#' @param wave      wavyness of curves defined in terms of x axis data range - experiment to get this right
#' @param ygap      gap between items on each y axis
#' @param col       a single colour or vector of colours for categories when listed in alphabetical order
#' @param leg.mode  if true legend plotted in largest data observation, otherwise custom coordinates (leg.x/y [0,1])
#' 
#' TODO: Add descriptions of remaining arguments above
#'
#' TODO: Some details here
#'
#' @export

alluvial_ts = function(dat, wave=NA, ygap=1, col=NA, plotdir='up', rankup=T, xmargin=1.1, lab.cex=1, 
                    title=NA, title.cex=1, xaxis.cex=1, grid=F, grid.col='grey80', grid.lwd=1, 
                    leg.mode=T, leg.x=.1, leg.y=.9, leg.cex=1, leg.col='black', leg.lty=NA, leg.lwd=NA,
                    leg.max=NA, lwd=NA, lwd.mult=1, lwd.max=1, ...){
  
  require(grid)
  require(scales)
  require(reshape2)
  orig.names = names(dat)
  names(dat) = c('item','time','val')
  if(is.ordered(dat$time) | is.factor(dat$time)) {
    axis.labs = levels(dat$time)
    dat$time = as.numeric(dat$time)
  } else if(is.numeric(dat$time)) axis.labs = sort(unique(dat$time)) else {
    return("Error: time variable must be numeric, factor, or ordered")}
  times = sort(unique(dat$time))
  
  dat = dat[order(dat$item),]
  datsum = aggregate(val ~ item, dat, mean)
  plotorder = order(datsum$val, decreasing=T) # smallest last (on top)
  maxval = pretty(max(dat$val))[2] # legend max
  
  # colours
  n = length(unique(dat$item))
  if(all(is.na(col))) col = substr(rainbow(n), 1, 7)
  if(length(col)==1) col = rep(col, n)
  
  # calc vertical gap between items
  ymean = mean(dat$val)
  ygap = ymean * .1 * ygap
  
  # if not specified (but it really should be)
  if(is.na(wave)) wave = .5 * (rev(times)[1] - times[1])/length(times)
  
  plot.y.max = 0  # vertical plot scaling
  
  # prepare main data object
  d = list()
  for(i in unique(dat$item)) d[[i]] = list()
  
  # loop through periods
  for(i in 1:length(times)){
    
    # 3 time variables, NA if they fall outside the data period
    if(i>1) t1 = times[i-1] else t1 = NA              # prev period
    t2 = times[i]                                     # this period
    if(i<length(times)) t3 = times[i+1] else t3 = NA  # next period
    
    dat.t = dat[dat$time == t2,]
    dat.t = if(rankup) dat.t[order(dat.t$val, decreasing=T),] else dat.t[order(dat.t$val, decreasing=F),]
    
    y.sum = sum(dat.t$val) + (nrow(dat.t) * ygap)
    if(y.sum > plot.y.max) plot.y.max = y.sum
    
    # loop through items
    if(plotdir=='centred') y = -y.sum/2 else y = 0  # vertical scaler
    
    for(j in 1:nrow(dat.t)){
      # work up/down y axes to calculate spline positions
      y0 = y + ygap
      y1 = y0 + dat.t$val[j]
      y = y1
      
      # calculate left and right x-axis splines
      if(!is.na(t1)) spline.x = c(t2 - wave, t2) else spline.x = numeric(0)
      if(!is.na(t3)) spline.x = c(spline.x, t2, t2 + wave)
      
      # update d
      item = as.character(dat.t$item[j])
      d[[item]]$x  = c(d[[item]]$x,  spline.x)
      d[[item]]$y0 = c(d[[item]]$y0, rep(y0, length(spline.x)))
      d[[item]]$y1 = c(d[[item]]$y1, rep(y1, length(spline.x)))
    } # end items loop
  } # end period loop
  
  # function to ensure vertex arrays are same length, as xspline output can vary
  resize = function(v, n=500){d=data.frame(x=1:length(v), y=v); approx(d, xout=seq(1,length(v), length.out=n))$y}
  plot.new() # required by xspline
  
  # calculate spline curves
  for(i in names(d)){
    curves = list()
    # iterate through bottom/top sets of bezier points, get curves and resize
    for(j in c('y0','y1')) curves[[j]] = lapply(xspline(d[[i]]$x, d[[i]][[j]], shape=1, draw=F), resize)
    # stitch top and bottom polylines together clockwise into a polygon
    d[[i]]$poly = data.frame(x=c(curves[[1]]$x, rev(curves[[2]]$x)), y=c(curves[[1]]$y, rev(curves[[2]]$y)))
  }
  
  # label y positions
  labs.l = data.frame(t(sapply(1:length(d), FUN = function(i)t(data.frame(lab=names(d[i]), lefty=mean(c(d[[i]]$y0[1], d[[i]]$y1[1]))) ))), stringsAsFactors=F)
  labs.r = data.frame(t(sapply(1:length(d), FUN = function(i)t(data.frame(lab=names(d[i]), lefty=mean(c(rev(d[[i]]$y0)[1], rev(d[[i]]$y1)[1])))) )), stringsAsFactors=F)
  names(labs.l) = names(labs.r) = c('item','y')
  labs.l$y = as.numeric(as.character(labs.l$y)); labs.r$y = as.numeric(as.character(labs.r$y)) # to numeric
  labs.l$col = col[match(labs.l$item, datsum$item)]
  labs.r$col = col[match(labs.r$item, datsum$item)]
  
  # line widths
  widths = aggregate(val ~ item, dat, mean)
  ext = function(fn, ext, vec) sapply(vec, function(i){fn(i,ext)} )
  if(is.na(lwd)) widths$val = ext(max, .01, ext(min, lwd.max, rescale(widths$val) * lwd.mult)) else widths$val = lwd
  
  # scale and orientation of axes
  if(plotdir == 'up')       ylim = c(0, plot.y.max) else {
    if(plotdir == 'down')   ylim = c(plot.y.max, 0) else {
      if(plotdir == 'centred') ylim = c(-plot.y.max/2, plot.y.max/2) else {
        return("Incorrect specification for plotdir: please select 'up', 'down' or 'centred'")
      }}}
  xran = range(dat$time)
  xlim = extendrange(r=xran, f=(xmargin-1)/2)
  
  d = d[plotorder]      # reorder to plot largest first
  col = col[plotorder]  # reorder colours to match
  
  # plot
  plot.window(xlim = xlim, ylim = ylim)
  axis(1, times, cex.axis=xaxis.cex, labels = axis.labs)
  if(is.na(title)) title(paste('Alluvial plot of', orig.names[1], 'vs', orig.names[3]), cex.main=title.cex) else title(title, cex.main=title.cex)
  if(grid) abline(v = times, col = grid.col, lwd = grid.lwd)
  for(i in 1:length(d)) polygon(d[[i]]$poly$x, d[[i]]$poly$y, col=col[i], lwd=widths$val[widths$item==names(d[i])], ...)
  text(times[1], labs.l$y, labels=labs.l$item, col=labs.l$col, pos=2, cex=lab.cex)
  text(times[length(times)], labs.r$y, labels=labs.r$item, adj=0, col=labs.r$col, pos=4, cex=lab.cex)
  
  # legend
  topval  = max(dat$val)
  topitem = as.character(dat$item[match(topval, dat$val)])
  toptime = dat$time[match(topval, dat$val)]
  
  if(leg.mode){ # legend plotted on maximum data point
    if(is.na(leg.lty)) leg.lty = "dotted"; if(is.na(leg.lwd)) leg.lwd=2
    val_ind = match(toptime, d[[topitem]]$x)
    leg_y0 = d[[topitem]]$y0[val_ind]
    leg_y1 = d[[topitem]]$y1[val_ind]
    leg_ym = mean(c(leg_y0, leg_y1))
    lines(rep(toptime,2), c(leg_y0, leg_ym-(topval*.08)), lwd=leg.lwd, lend='butt', col=leg.col, lty=leg.lty)
    lines(rep(toptime,2), c(leg_y1, leg_ym+(topval*.08)), lwd=leg.lwd, lend='butt', col=leg.col, lty=leg.lty)
    text(toptime, leg_ym, labels=formatC(topval,format="d",big.mark=','), pos=NULL, cex=leg.cex, col=leg.col)
  } else {         # legend plotted in custom position
    if(!is.na(leg.max)) maxval = leg.max
    if(is.na(leg.lty)) leg.lty = "solid"; if(is.na(leg.lwd)) leg.lwd=10
    leg_x = (xlim[2]-xlim[1]) * leg.x + xlim[1]
    leg_y = (ylim[2]-ylim[1]) * leg.y + ylim[1]
    diffs = d[[topitem]]$y1 - d[[topitem]]$y0
    time = d[[topitem]]$x[match(max(diffs), diffs)]
    lines(data.frame(x = c(leg_x, leg_x), y = c(leg_y, leg_y + maxval)), lwd=leg.lwd, lend='butt', col=leg.col, lty=leg.lty)
    max.lab = formatC(maxval, format="d", big.mark=',')
    text(rep(leg_x, 2), c(leg_y, leg_y + maxval), labels = c('0', max.lab), cex=leg.cex, pos=4, offset = 1, col=leg.col)
  }
}
 
