alois.panel.levelplot <- function (x, y, z, subscripts, at = pretty(z), shrink, labels = FALSE, 
                                   label.style = c("mixed", "flat", "align"), contour = FALSE, 
                                   region = TRUE, col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
                                   ..., col.regions = regions$col, alpha.regions = regions$alpha,
                                   border = "transparent") 
{
  if (length(subscripts) == 0) 
    return()
  regions <- trellis.par.get("regions")
  label.style <- match.arg(label.style)
  x.is.factor <- is.factor(x)
  y.is.factor <- is.factor(y)
  x <- as.numeric(x)
  y <- as.numeric(y)
  z <- as.numeric(z)
  zcol <- level.colors(z, at, col.regions, colors = TRUE)
  x <- x[subscripts]
  y <- y[subscripts]

  print (length(unique(y)))
  minXwid <- if (length(unique(x)) > 1) 
    min(diff(sort(unique(x))))
  else 1
  minYwid <- if (length(unique(y)) > 1) 
    min(diff(sort(unique(y))))
  else 1
  fullZrange <- range(as.numeric(z), finite = TRUE)
  z <- z[subscripts]
  zcol <- zcol[subscripts]
  shrinkx <- c(1, 1)
  shrinky <- c(1, 1)
  if (!missing(shrink)) {
    if (is.numeric(shrink)) {
      shrinkx <- rep(shrink, length.out = 2)
      shrinky <- rep(shrink, length.out = 2)
    }
    else if (is.list(shrink)) {
      shrinkx <- rep(shrink[[1]], length.out = 2)
      shrinky <- rep(shrink[[1]], length.out = 2)
      if ("x" %in% names(shrink)) 
        shrinkx <- rep(shrink$x, length.out = 2)
      if ("y" %in% names(shrink)) 
        shrinky <- rep(shrink$y, length.out = 2)
    }
    else warning("Invalid 'shrink' parameter ignored")
  }
  scaleWidth <- function(z, min = 0.8, max = 0.8, zl = range(z, 
                                                    finite = TRUE)) {
    if (diff(zl) == 0) 
      rep(0.5 * (min + max), length(z))
    else min + (max - min) * (z - zl[1])/diff(zl)
  }
  if (x.is.factor) {
    ux <- sort(unique(x[!is.na(x)]))
    lx <- rep(1, length(ux))
    cx <- ux
  }
  else {
    ux <- sort(unique(x[!is.na(x)]))
    bx <- if (length(ux) > 1) 
      c(3 * ux[1] - ux[2], ux[-length(ux)] + ux[-1], 3 * 
        ux[length(ux)] - ux[length(ux) - 1])/2
    else ux + c(-0.5, 0.5) * minXwid
    lx <- diff(bx)
    cx <- (bx[-1] + bx[-length(bx)])/2
  }
  if (y.is.factor) {
    uy <- sort(unique(y[!is.na(y)]))
    ly <- rep(1, length(uy))
    cy <- uy
  }
  else {
    uy <- sort(unique(y[!is.na(y)]))
    by <- if (length(uy) > 1) 
      c(3 * uy[1] - uy[2], uy[-length(uy)] + uy[-1], 3 * 
        uy[length(uy)] - uy[length(uy) - 1])/2
    else uy + c(-0.5, 0.5) * minYwid
    ly <- diff(by)
    cy <- (by[-1] + by[-length(by)])/2
  }
  idx <- match(x, ux)
  idy <- match(y, uy)
  if (region) 
    grid.rect(x = cx[idx], y = cy[idy], width = lx[idx] * 
              scaleWidth(z, shrinkx[1], shrinkx[2], fullZrange), 
              height = ly[idy] * scaleWidth(z, shrinky[1], shrinky[2], 
                fullZrange), default.units = "native", gp = gpar(fill = zcol, 
                                                         lwd = if (border == "transparent") 0.00001 else 1,
                                                         col = border, alpha = alpha.regions))
  if (contour) {
    cpl <- current.panel.limits(unit = "cm")
    asp <- diff(cpl$ylim)/diff(cpl$xlim)
    if (is.logical(labels) && !labels) 
      labels <- NULL
    else {
      if (is.characterOrExpression(labels)) 
        labels <- list(labels = labels)
      text <- trellis.par.get("add.text")
      tmp <- list(col = text$col, alpha = text$alpha, cex = text$cex, 
                  fontfamily = text$fontfamily, fontface = text$fontface, 
                  font = text$font)
      labels <- if (is.list(labels)) 
        updateList(tmp, labels)
      else tmp
      if (!is.characterOrExpression(labels$labels)) 
        labels$labels <- format(at, trim = TRUE)
    }
    add.line <- trellis.par.get("add.line")
    m <- matrix(NA_real_, nrow = length(ux), ncol = length(uy))
    m[(idy - 1) * length(ux) + idx] <- z
    clines <- contourLines(x = ux, y = uy, z = m, nlevels = length(at), 
                           levels = at)
    for (val in clines) {
      llines(val, col = col, lty = lty, lwd = lwd)
      if (length(val$x) > 5) {
        if (!is.null(labels)) {
          slopes <- diff(val$y)/diff(val$x)
          if (label.style == "flat") {
            textloc <- which.min(abs(slopes))
            rotangle <- 0
          }
          else if (label.style == "align") {
            rx <- range(ux)
            ry <- range(uy)
            depth <- pmin(pmin(val$x - rx[1], rx[2] - 
                               val$x)/diff(rx), pmin(val$y - ry[1], ry[2] - 
                                                     val$y)/diff(ry))
            textloc <- min(which.max(depth), length(slopes))
            rotangle <- atan(asp * slopes[textloc] * 
                             diff(rx)/diff(ry)) * 180/base::pi
          }
          else if (label.style == "mixed") {
            rx <- range(ux)
            ry <- range(uy)
            depth <- pmin(pmin(val$x - rx[1], rx[2] - 
                               val$x)/diff(rx), pmin(val$y - ry[1], ry[2] - 
                                                     val$y)/diff(ry))
            textloc <- which.min(abs(slopes))
            rotangle <- 0
            if (depth[textloc] < 0.05) {
              textloc <- min(which.max(depth), length(slopes))
              rotangle <- atan(asp * slopes[textloc] * 
                               diff(rx)/diff(ry)) * 180/base::pi
            }
          }
          else stop("Invalid label.style")
          i <- match(val$level, at)
          ltext(labels$labels[i], adj = c(0.5, 0), srt = rotangle, 
                col = labels$col, alpha = labels$alpha, cex = labels$cex, 
                font = labels$font, fontfamily = labels$fontfamily, 
                fontface = labels$fontface, x = 0.5 * (val$x[textloc] + 
                                              val$x[textloc + 1]), y = 0.5 * (val$y[textloc] + 
                                                                     val$y[textloc + 1]))
        }
      }
    }
  }
}

i <- sample (nrow (chondro), nrow (chondro) / 4)

system.time(print (
                   plotmap (chondro [i], z = "clusters", col.regions = matlab.palette (),
                            panel = alois.panel.levelplot, border = "#00000040"
                            )
                   ))
plotmap (chondro , z = "clusters", col.regions = matlab.palette (),
         panel = alois.panel.levelplot, border = "#00000040"
         )

un.x <- unique (chondro$x)
un.y <- unique (chondro$y)
plotmap (chondro [chondro$x %in%  un.x[seq (2, length (un.x), 2)] &
                  chondro$y %in%  un.y[seq (1, length (un.y), 2)] ],
         z = "clusters", col.regions = matlab.palette (),
         panel = alois.panel.levelplot, border = "#00000040"
         )

df <-         chondro [chondro$x %in%  un.x[seq (1, length (un.x), 2)] &
                       chondro$y %in%  un.y[seq (1, length (un.y), 2)] ,c("x", "y", "clusters")]$..

xyplot (y ~ x,
        df,
        col = matlab.dark.palette (3)[df$clusters],
        pch = 20, cex = 1.5,
        aspect = "iso"
                                        #        legend = list (right = list (fun = draw.colorkey,
                                        #                        args = list (key = list (col = matlab.palette (3),
                                        #                                      at = pretty (df$clusters, 20))))))
        )


system.time(print (
                   xyplot (y ~ x,
                                        #as.long.df (apply (chondro [i], 1, mean)),
                           chondro[i,c("x", "y", "clusters")]$..,
                           col = level.colors (df$spc, pretty (df$spc, 20), matlab.darpalette (100)),
                           pch = 20, cex = 2,
                           aspect = "iso",
                           legend = list (right = list (fun = draw.colorkey,
                                            args = list (key = list (col = matlab.palette (100),
                                                           at = pretty (df$spc))))))
                   ))


system.time(print (
                   tileplot (clusters ~ x * y,
                                        #as.long.df (apply (chondro [i], 1, mean)),
                             chondro[i,c("x", "y", "clusters")]$..,
                             col.regions = matlab.palette (100), #colorRampPalette (c("dark blue", "white", "dark red"), space = "Lab") (20),
                             border = "#00000020", pch = 19, cex = 0.25, col = "black",
                             prepanel = prepanel.default.levelplot)
                   )
            )
)
system.time(print (
tileplot (spc ~ x * y,
          as.long.df (apply (chondro , 1, mean)),
          col.regions = colorRampPalette (c("dark blue", "#FFFFA0", "dark red"), space = "Lab") (20),
          border = "#00000020", pch = 19, cex = 0.25, col = "black",
          prepanel = prepanel.default.levelplot)
                   )
            )
         )



t <- list()
n <-  c (seq (10, 1010, 100), 2000, 3000, 4000, 5000)
n <- lapply (n, function (x) sample (875, x, replace=TRUE))

for (i in 12 : 15) {
  df <- apply (chondro [n [[i]]], 1, mean)
  df$x <- df$x + runif (length (n[[i]])) / 10
  df$y <- df$y + runif (length (n[[i]])) / 10
  df <- as.long.df (df)
  t [[i]] <- system.time (print (tileplot (spc ~ x * y, df,
  

#                                           col.regions = colorRampPalette (c("dark blue", "white", "dark red"), space = "Lab") (20),
                                          border = "#00000020", pch = 19, cex = 0.25, col = "black",
                                         prepanel = prepanel.default.levelplot)
                                 )
                          )
                          }


data <- data.frame (n=sapply(n, length), t = sapply(t[1:15], `[`, 1))
plot(data)
summary (lm (t ~ n, data))
abline (lm (t ~ n, data))
summary (lm (t ~ n + I(n^2), data))

t [[15]]
system.time (print (plotmap (chondro, col.regions = colorRampPalette (c("dark blue", "white"), space = "Lab") (20))))

hex.x <- 1:20
hex.y <- (1:23) * sin (pi / 3)

pos <- data.frame (x = rep (hex.x, each = length (hex.y)),
                   y = rep (hex.y,        length (hex.x)))
pos$x [pos$y %in% hex.y [seq (1, length (hex.y), 2)]] <- pos$x [pos$y %in% hex.y [seq (1, length (hex.y), 2)]] + 0.5

xyplot(y ~ x, pos, aspect = "iso")

pos$z <- seq_along (pos$x)

tileplot (z ~ x * y, pos, border = "black", pch = 19, cex = 0.25, prepanel = prepanel.default.levelplot)

