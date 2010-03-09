triax.points <- function (x, show.legend = FALSE, label.points = FALSE, point.labels = NULL, 
    col.symbols = par("fg"), pch = par("pch"), bg.symbols = par("bg"), 
    cc.axes = FALSE, ...) 
{
  x <- tri2cart (x, cc.axes)
  nobs <- dim(x)[1]
  points(x = x [, 1], y = x [, 2], pch = pch, col = col.symbols, 
         bg = bg.symbols, type = "p", ...)
  if (is.null(point.labels)) 
    point.labels <- rownames(x)
  if (label.points) 
    thigmophobe.labels(x [, 1], x [, 2], point.labels)
  if (show.legend) {
    legend(0.2, 0.7, legend = point.labels, pch = pch, col = col.symbols, 
           xjust = 1, yjust = 0)
  }
  invisible(list(x = x [, 1], y = x [, 2]))
}

triax.plot <- function (x = NULL, main = "", at = seq(0.1, 0.9, by = 0.1), 
    axis.labels = NULL, tick.labels = NULL, col.axis = "black", 
    cex.axis = 1, cex.ticks = 1, align.labels = TRUE, show.grid = FALSE, 
    col.grid = "gray", lty.grid = par("lty"), cc.axes = FALSE, 
    show.legend = FALSE, label.points = FALSE, point.labels = NULL, 
    col.symbols = "black", pch = par("pch"), no.add = TRUE, cex.symbols = 1, ...) 
{
    oldpar <- par(no.readonly = TRUE)
    par(xpd = TRUE)
    if (is.null(axis.labels)) 
        axis.labels <- colnames(x)[1:3]

    rng.cart <- apply (tri2cart (x), 2, range)
    rng.cart [1, ] <- sapply (rng.cart [1, ], min, 0)
    rng.cart [2, ] <- sapply (rng.cart [2, ], max, 1)
    
    triax.frame(main = main, at = at, axis.labels = axis.labels, 
        tick.labels = tick.labels, col.axis = col.axis, cex.axis = cex.axis, 
        cex.ticks = cex.ticks, align.labels = align.labels, show.grid = show.grid, 
        col.grid = col.grid, lty.grid = lty.grid, cc.axes = cc.axes,
                xlim = rng.cart [, 1], ylim = rng.cart [, 2]
                )
    if (is.null(x)) 
        xypos <- NULL
    else xypos <- triax.points(x, show.legend = show.legend, 
        label.points = label.points, point.labels = point.labels, 
        col.symbols = col.symbols, pch = pch, cc.axes = cc.axes,
                               cex = cex.symbols,
        ...)
    if (no.add) 
        par(oldpar)
    invisible(list(xypos = xypos, oldpar = oldpar))
}

triax.frame <- function (main = "", at = seq(0.1, 0.9, by = 0.1), axis.labels = NULL, 
    tick.labels = NULL, col.axis = "black", cex.axis = 1, cex.ticks = 1, 
    align.labels = TRUE, show.grid = FALSE, col.grid = "gray", 
    lty.grid = par("lty"), cc.axes = FALSE,
          xlim = 0 : 1, ylim = 0 : 1) 
{
    par(pty = "s", mar = c(5, 2, 4, 2))
    plot(0.5, type = "n", axes = FALSE, xlim = xlim, ylim = ylim, main = main, xlab = "", ylab = "")
    sin60 <- sin(pi/3)
    bx1 <- at
    bx2 <- bx1 + 0.01 - 0.02 * cc.axes
    by1 <- rep(0, 9)
    if (cc.axes) 
        by2 <- rep(-0.02 * sin(2 * pi/3), 9)
    else by2 <- rep(-0.02 * sin60, 9)
    ly1 <- at * sin60
    lx1 <- bx1 * 0.5
    lx2 <- lx1 - 0.02 + 0.013 * cc.axes
    if (cc.axes) 
        ly2 <- ly1 + rep(0.014 * sin60, 9)
    else ly2 <- ly1
    rx1 <- at * 0.5 + 0.5
    rx2 <- rx1 + 0.01
    ry1 <- rev(ly1)
    if (cc.axes) 
        ry2 <- ry1
    else ry2 <- rev(ly2) + 0.02 * sin60
    if (show.grid) {
        par(fg = col.grid)
        segments(bx1, by1, lx1, ly1, lty = lty.grid)
        segments(lx1, ly1, rev(rx1), rev(ry1), lty = lty.grid)
        segments(rx1, ry1, bx1, by1, lty = lty.grid)
    }
    par(fg = col.axis, xpd = TRUE)
    if (is.null(tick.labels)) {
        if (cc.axes) 
            tick.labels <- list(l = rev(at), r = rev(at), b = rev(at))
        else tick.labels <- list(l = at, r = at, b = at)
    }
    else {
        if (cc.axes) {
            tick.labels$l <- rev(tick.labels$l)
            tick.labels$r <- rev(tick.labels$r)
            tick.labels$b <- rev(tick.labels$b)
        }
    }
    if (align.labels) 
        par(srt = 60)
    text(0.13, 0.5, axis.labels[3 - cc.axes], adj = 0.5, cex = cex.axis)
    if (cc.axes) {
        par(srt = 300)
        xoffset <- 0.02
        yoffset <- 0.04
    }
    else {
        par(srt = 0)
        xoffset <- 0.05
        yoffset <- 0
    }
    text(lx1 - xoffset, ly1 + yoffset, tick.labels$l, cex = cex.ticks)
    if (align.labels) {
        par(srt = 300)
        label.adj <- 0.5
    }
    else {
        par(srt = 0)
        label.adj <- 0
    }
    text(0.86, 0.52, axis.labels[2 + cc.axes], adj = label.adj, 
        cex = cex.axis)
    if (cc.axes) {
        par(srt = 0)
        xoffset <- 0.033
        yoffset <- 0.005
    }
    else {
        par(srt = 60)
        xoffset <- 0.015
        yoffset <- 0.045
    }
    text(rx2 + xoffset, ry1 + yoffset, tick.labels$r, cex = cex.ticks)
    if (cc.axes) {
        par(srt = 60)
        xoffset <- -0.03
    }
    else {
        par(srt = 300)
        xoffset <- 0.03
    }
    text(bx1 + xoffset, by1 - 0.05, rev(tick.labels$b), cex = cex.ticks)
    par(srt = 0)
    text(0.5, -0.14, axis.labels[1], cex = cex.axis)
    x1 <- c(0, 0, 0.5)
    x2 <- c(1, 0.5, 1)
    y1 <- c(0, 0, sin60)
    y2 <- c(0, sin60, 0)
    par(fg = col.axis)
    segments(x1, y1, x2, y2)
    segments(bx1, by1, bx2, by2)
    segments(lx1, ly1, lx2, ly2)
    segments(rx1, ry1, rx2, ry2)
}


tri2cart <- function (x, cc.axes = FALSE) {
  sin60 <- sin(pi/3)
  
  ypos <- x[, 3] * sin60
  xpos <- x[, 1] + x[, 3] * 0.5
  if (!cc.axes)
    xpos <- 1 - xpos

  matrix (c (xpos, ypos), ncol = 2)
}

triax.hist <- function (x, y = NULL, z = NULL, nbins = 10, cc.axes = FALSE){
stop ("buggy")
  if (xor (is.null (y), is.null (z)))
    stop ('y and z must be either both given or x must have 3 columns')
  if (! is.null (y) && !is.null (z)) {
    x <- matrix (c (x, y, z), ncol = 3)
  } else if (ncol (x) != 3)
    stop ('y and z must be either both given or x must have 3 columns')
    
  rng <- apply (x, 2, range) 
  rng.bin <- rng * nbins
  rng.bin  [1, ] <- floor (rng.bin [1, ]) - 1
  rng.bin  [2, ] <- ceiling (rng.bin [2, ]) + 1
  delta <- 0.5 / nbins
  
  bx <- (rng.bin [1, 1] : rng.bin [2, 1]) / nbins
  by <- (rng.bin [1, 2] : rng.bin [2, 2]) / nbins

  bins <- matrix (NA_real_, nrow = length (bx) * length (by), ncol = 3)
  bins [, 1 : 2] <- as.matrix (expand.grid (bx, by))
  bins [, 3] <- 1 - bins [, 1] - bins [, 2]
  print (dim(bins))
#  bins <- bins [bins [, 3] >= rng [1, 3] - delta & bins [, 3] <= rng [2, 3] + delta, ]

  print (apply(bins, 2, range))

  par(triax.plot (bins, pch = 16, cex.symbols = 0, cc.axes = cc.axes, no.add=FALSE)$oldpar)

  count <- function (bin, x)
    sum (x [, 1] >= bin [1] - delta & x [, 1] < bin [1] + delta &
         x [, 2] >= bin [2] - delta & x [, 2] < bin [2] + delta &
         x [, 3] >= bin [3] - delta & x [, 3] < bin [3] + delta
         )
  counted <- function (bin, x){
    x [, 1] >= bin [1] - delta & x [, 1] < bin [1] + delta &
    x [, 2] >= bin [2] - delta & x [, 2] < bin [2] + delta &
    x [, 3] >= bin [3] - delta & x [, 3] < bin [3] + delta
  }
  
  counts <- apply (bins, 1, count, x)

  
  print (sum (counts))
  
  pbins <- tri2cart (bins, cc.axes)
  text (pbins [,1], pbins [,2], as.character (counts))
  
}
