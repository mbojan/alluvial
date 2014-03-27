#' Alluvial diagram
#'
#' Drawing alluvial diagrams also known as parallel set plots.
#'
#' Still under development!
#'
#' @param ... vectors or data frames, all for the same number of observations
#' @param freq numeric, vector of frequencies of the same length as the number of observations
#' @param col vector of colors of the stripes
#' @param border vector of border colors for the stripes
#' @param layer numeric, order of drawing of the stripes
#' @param hide logical, should particular stripe be plotted
#' @param alpha numeric, vector of transparency of the stripes
#' @param gap.width numeric, relative width of inter-category gaps
#' @param xw numeric, the distance from the set axis to the control points of the xspline
#' @param cw numeric, width of the category axis
#'
#' @return Nothing
#'
#' @export
#'
#' @example examples/alluvial.R

alluvial <- function( ..., freq, col="gray", border=0, layer, hide=FALSE, alpha=0.5,
                     gap.width=0.05, xw=0.1, cw=0.1 )
{
  # Data and graphical parameters
  p <- data.frame( ..., freq=freq, col, alpha, border, hide, stringsAsFactors=FALSE)
  n <- nrow(p)
  # Layers determine plotting order
  if(missing(layer))
  {
    layer <- 1:n
  }
  p$layer <- layer
  np <- ncol(p) - 6                    # Number of dimensions
  d <- p[ , 1:np, drop=FALSE]          # Dimensions dframe
  p <- p[ , -c(1:np), drop=FALSE]      # Parameteres dframe
  p$freq <- with(p, freq/sum(freq))    # Frequencies (weights)
  # Converting colors to hexcodes
  col <- col2rgb(p$col, alpha=TRUE)
  if(!identical(alpha, FALSE)) {
    col["alpha", ] <- p$alpha*256
  }
  p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x), maxColorValue = 256)))
  # Compute endpoints of flows (polygons)
  # i = dimension id
  # d = data frame of dimensions
  # f = weights
  # w = gap between categories
  getp <- function(i, d, f, w=gap.width) {
    # Ordering dimension ids for lexicographic sorting
    a <- c(i, (1:ncol(d))[-i])
    # Order of rows of d starting from i-th dimension
    o <- do.call(order, d[a])
    # Breakpoints on a dimension
    x <- c(0, cumsum(f[o])) * (1-w)
    # Stripe coordinates on a dimension
    x <- cbind(x[-length(x)], x[-1])
    # By how much stripes need to be shifted upwards (gap/max(gap))
    gap <- cumsum( c(0L, diff(as.numeric(d[o,i])) != 0) )
    # shifts
    gap <- gap / max(gap) * w
    # add gap-related shifts to stripe coordinates on dimension i
    (x + gap)[order(o),]
  }
  # Calculate stripe locations on dimensions: list of data frames. A component
  # for a dimension. Data frame contains 'y' locations of stripes.
  dd <- lapply(seq_along(d), getp, d=d, f=p$freq)
  # Plotting
  op <- par(mar=c(2, 1, 1, 1))
  plot(NULL, type="n", xlim=c(1-cw, np+cw), ylim=c(0, 1), xaxt="n", yaxt="n",
       xaxs="i", yaxs="i", xlab='', ylab='', frame=FALSE)
  # For every stripe
  ind <- rev(order(p$layer)[!hide])
  for(i in ind )
  {
    # For every inter-dimensional segment
    for(j in 1:(np-1) )
    {
      # Draw stripe
      xspline( c(j, j, j+xw, j+1-xw, j+1, j+1, j+1-xw, j+xw, j) + rep(c(cw, -cw, cw), c(3, 4, 2)),
             c( dd[[j]][i, c(1, 2, 2)], rev(dd[[j+1]][i, c(1, 1, 2, 2)]), dd[[j]][i,c(1, 1)]), 
             shape = c(0,0,1,1,0,0,1,1,0, 0),
             open=FALSE,
             col=p$col[i], border=p$border[i])
    }
  }
  # Category blocks with labels
  for(j in seq_along(dd))
  {
    ax <- lapply(split(dd[[j]], d[,j]), range)
    for(k in seq_along(ax))
    {
      rect( j-cw, ax[[k]][1], j+cw, ax[[k]][2] )
      text( j, mean(ax[[k]]), labels=names(ax)[k])
    }
  }           
  # X axis
  axis(1, at= rep(c(-cw, cw), ncol(d)) + rep(seq_along(d), each=2),
       line=0.5, col="white", col.ticks="black", labels=FALSE)
  axis(1, at=seq_along(d), tick=FALSE, labels=names(d))
  par(op)
}

