#' Alluvial diagram
#'
#' Drawing alluvial diagrams, also known as parallel set plots.
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
#' @param blocks logical, whether to use blocks to tie the flows together at each category, versus contiguous ribbons (also admits character value "bookends")
#' @param ordering list of numeric vectors allowing to reorder the alluvia on each axis separately, see Examples
#' @param axis_labels character, labels of the axes, defaults to variable names in the data
#' @param mar numeric, plot margins as in \code{\link{par}}
#' @param cex,cex.axis numeric, scaling of fonts of category labels and axis labels respectively. See \code{\link{par}}.
#' @param xlim_offset,ylim_offset numeric vectors of length 2, passed to
#'   \code{xlim} and \code{ylim} of \code{\link{plot}}, and allow for adjusting
#'   the limits of the plotting region
#' @param axes logical, whether to draw axes, defaults to TRUE
#' @param ann logical, whether to draw annotations: category labels. Defaults to TRUE
#' @param title character, plot title
#'
#' @return Invisibly a list with elements:
#' \item{\code{endpoints}}{A data frame with data on locations of the stripes with columns:
#' \describe{
#' \item{\code{...}}{Vectors/data frames supplied to
#' \code{alluvial} through \code{...} that define the axes}
#' \item{\code{.bottom},\code{.top}}{Y locations of bottom
#' and top coordinates respectively at which the stripes
#' originate from the axis \code{.axis}}
#' \item{\code{.axis}}{Axis number counting from the left}
#' } }
#' \item{\code{category_midpoints}}{List of vectors of Y
#' locations of category block midpoints.}
#' 
#' @note Please mind that the API is planned to change to be more compatible
#'   with \pkg{dplyr} verbs.
#' 
#' @importFrom grDevices col2rgb rgb
#' @importFrom graphics plot xspline axis rect polygon text par
#'
#' @export
#'
#' @example man-roxygen/alluvial.R

alluvial <- function( ..., freq,
                     col="gray", border=0, layer, hide=FALSE, alpha=0.5,
                     gap.width=0.05, xw=0.1, cw=0.1,
                     blocks = TRUE,
                     ordering=NULL,
                     axis_labels=NULL,
                     mar = c(2, 1, 1, 1),
                     cex=par("cex"),
                     xlim_offset= c(0, 0),
                     ylim_offset= c(0, 0),
                     cex.axis=par("cex.axis"),
                     axes=TRUE,
                     ann=TRUE,
                     title = NULL)
{
  # Data and graphical parameters
  p <- data.frame( ..., freq=freq, col, alpha, border, hide, stringsAsFactors=FALSE)
  np <- ncol(p) - 5                    # Number of dimensions
  # check if 'ordering' is of proper form
  if( !is.null(ordering) )
  {
    stopifnot(is.list(ordering))
    if( length(ordering) != np )
      stop("'ordering' argument should have ",
           np, " components, has ", length(ordering))
  }
  n <- nrow(p)
  # Layers determine plotting order
  if(missing(layer))
  {
    layer <- 1:n
  }
  p$layer <- layer
  d <- p[ , 1:np, drop=FALSE]          # Dimensions dframe
  p <- p[ , -c(1:np), drop=FALSE]      # Parameteres dframe
  p$freq <- with(p, freq/sum(freq))    # Frequencies (weights)
  # Converting colors to hexcodes
  col <- col2rgb(p$col, alpha=TRUE)
  if(!identical(alpha, FALSE)) {
    col["alpha", ] <- p$alpha*256
  }
  p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x), maxColorValue = 256)))
  # convert character vectors in data to factors
  isch <- sapply(d, is.character)
  d[isch] <- lapply(d[isch], as.factor)
  # Convert blocks to vector
  if (length(blocks) == 1)
  {
    blocks <- if (!is.na(as.logical(blocks)))
    {
      rep(blocks, np)
    } else if (blocks == "bookends")
    {
      c(TRUE, rep(FALSE, np - 2), TRUE)
    }
  }
  # Axis labels
  if(is.null(axis_labels)) {
    axis_labels <- names(d)
  } else {
    if(length(axis_labels) != ncol(d))
      stop("`axis_labels` should have length ", names(d), ", has ", length(axis_labels))
  }
  # Compute endpoints of flows (polygons)
  # i = dimension id
  # d = data frame of dimensions
  # f = weights
  # w = gap between categories
  getp <- function(i, d, f, w=gap.width) {
    # Ordering dimension ids for lexicographic sorting
    a <- c(i, (1:ncol(d))[-i])
    # Order of rows of d starting from i-th dimension
    if( is.null(ordering[[i]]) )
    {
      o <- do.call(order, d[a])
    } else {
      d2 <- d
      d2[1] <- ordering[[i]]
      o <- do.call(order, d2[a])
    }
    # Breakpoints on a dimension
    x <- c(0, cumsum(f[o])) * (1-w)
    # Stripe coordinates on a dimension
    x <- cbind(x[-length(x)], x[-1])
    # By how much stripes need to be shifted upwards (gap/max(gap))
    gap <- cumsum( c(0L, diff(as.numeric(d[o,i])) != 0) )
    mx <- max(gap)
    if (mx == 0) mx <- 1
    # shifts
    gap <- gap / mx * w
    # add gap-related shifts to stripe coordinates on dimension i
    (x + gap)[order(o),]
  }
  # Calculate stripe locations on dimensions: list of data frames. A component
  # for a dimension. Data frame contains 'y' locations of stripes.
  dd <- lapply(seq_along(d), getp, d=d, f=p$freq)
  # Plotting
  op <- par(mar=mar)
  plot(NULL, type="n", xlim=c(1-cw, np+cw) + xlim_offset, ylim=c(0, 1) + ylim_offset, xaxt="n", yaxt="n",
       xaxs="i", yaxs="i", xlab='', ylab='', main=title, frame=FALSE)
  # For every stripe
  ind <- which(!p$hide)[rev(order(p[!p$hide, ]$layer))]
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
    if (blocks[j])
    {
      for(k in seq_along(ax))
      {
        rect( j-cw, ax[[k]][1], j+cw, ax[[k]][2] )
      }
    } else
    {
      for (i in ind)
      {
        x <- j + c(-1, 1) * cw
        y <- t(dd[[j]][c(i, i), ])
        w <- xw * (x[2] - x[1])
        xspline(x = c(x[1], x[1], x[1] + w, x[2] - w,
                      x[2], x[2], x[2] - w, x[1] + w, x[1]),
                y = c(y[c(1, 2, 2), 1], y[c(2, 2, 1, 1), 2], y[c(1, 1), 1]),
                shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
                open = FALSE, col = p$col[i], border = p$border[i])
      }
    }
    for(k in seq_along(ax))
    {
      if(ann) text( j, mean(ax[[k]]), labels=names(ax)[k], cex=cex)
    }
  }           
  # X axis
  if(axes) {
    axis(1, at= rep(c(-cw, cw), ncol(d)) + rep(seq_along(d), each=2),
         line=0.5, col="white", col.ticks="black", labels=FALSE)
    axis(1, at=seq_along(d), tick=FALSE, labels=axis_labels, cex.axis=cex.axis)
  }
  par(op)
  rval <- list(
    # Endpoints of alluvia
    endpoints = do.call(
      "rbind",
      lapply(
        seq(along=dd),
        function(i) {
          df <- as.data.frame(
            structure(dd[[i]], dimnames=list(NULL,c(".bottom", ".top"))),
            stringsAsFactors=FALSE
          )
          df$.axis <- i
          cbind(d, df)
        }
      )
    )
  )
  # Category midpoints
  rval$category_midpoints <- structure(
    lapply(
      seq(1, ncol(d)),
      function(i) {
        mi <- with(
          subset(rval$endpoints, .axis == i),
          tapply(.bottom, d[[i]], min)
        )
        ma <- with(
          subset(rval$endpoints, .axis == i),
          tapply(.bottom, d[[i]], max)
        )
        (mi + ma)/2
      }
    ),
    names = names(d)
  )
  invisible(rval)
}

