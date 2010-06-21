colmix.rgb <- function (x, purecol, against = 1, min = 0, max = 1, sub = TRUE){
  if (is.character (purecol))
    purecol <- t (col2rgb (purecol)) / 255

  if (sub)
    x <- against - x %*% (against - purecol)
  else
    x <- x %*% purecol
  
  x [x < min] <- min
  x [x > max] <- max

  cols <- rep (NA, nrow (x))
  cols [! is.na (x [,1])] <-   rgb (x [!is.na (x [, 1]),])

  cols
}

##' color mixing by lookup
##' 
##' looks the colors up in a table. Multivariate level.colors.
##' 
##' @aliases colmix.lookup
##' @author C. Beleites
##' @seealso 
##' @export 
##' @callGraph
##' @keywords plot
##' @examples 
##' 
##' @log C. Beleites 2010-05-27: 
##' @param x mixture
##' @param col.list list with color tables, one vector for each column of x
##' @param at cut marks
##' @param scale should the colors be individually scaled for each column of x
##' @param open should the color intervals be open? 
##' @param cuts the cut marks. must be n - 1 to n + 1 (depending on open) for n entries in col.list
#' [[i]].
colmix.lookup <- function (x, col.list = stop ("col.list must be given"),
                           at, scale.individual = TRUE, open = c (TRUE, TRUE)){
  if (!is.matrix (x))
    x <- as.matrix (x)
  
  if (missing (at))
    if (scale.individual) {
      at <- list ()
      for (i in 1 : ncol (x)){
        if (is.function (col.list [[i]]))
          ncolors <- 20
        else
          ncolors <- length (col.list [[i]])
        at [[i]] <- pretty (x [, i], n = ncolors - 1)
      }
    } else
      at <- list (pretty (x, n = 20))[rep (1, ncol (x))]
 
  ncolors <- sapply (at, length)
  
  for (i in 1 : ncol (x)){
    if (is.function (col.list [[i]]))
      col.list [[i]] <- col.list [[i]] (ncolors [i])
    else                                # interpolate
      col.list  [[i]] <- colorRampPalette (col.list [[i]])(ncolors [i])

    if (is.character (col.list [[i]]))
      col.list [[i]] <- col2rgb (col.list [[i]])

    ## finding the data intervals
    if (! open [2])
      x [x [,i] > max (at [[i]]), i] <- NA

    x [, i] <- findInterval (x [, i], at [[i]])
  }
  
  if (! open [1])
    x [x == 0] <- NA
  else
    x [x == 0] <- 1
   
  zcol <- array (NA, dim = c(3, dim (x)))

  for (i in 1 : ncol (x))
    zcol [, , i] <- col.list [[i]][, x [, i]]
  dim (zcol) <- c(3, dim (x))
  zcol <- aperm (zcol, c (3, 2, 1))
  zcol <- colSums (zcol) / ncol (x)
  dim (zcol) <- c (nrow (x), 3) 
  rgb (zcol / 255)
}

colmix.lookup <- NULL

zcolors <- function (z, colors, scale.individual = TRUE, enh = function (x) x, ...){
    if (scale.individual){
      z <- sweep (z, 2, apply (z, 2, min), `-`)
      z <- sweep (z, 2, apply (z, 2, max), `/`)
    } else {
      z <- z - min (z)
      z <- z / max (z)
    } 
      
    colmix.rgb (enh (z), colors)
}

panel.mixlevelplot <- function (x, y, z, subscripts, shrink, 
    border = "transparent", ..., col.regions = trellis.par.get ("regions")$col) 
{
    if (length(subscripts) == 0) 
        return()

    ## fix subscripts for matrices
    if (is.matrix (z))
      subscripts <- unique (row (z) [subscripts])

    if (is.factor (z) & all (is.na (as.numeric (levels (z))))){ # character wurden zu factor umgewandelt
      zcol <- as.character (z)
      z <- as.numeric (z)
    } else {
      zcol <- zcolors (z, col.regions, ...)
    }

    ncol <- ncol (z)
    if (is.null (ncol)) ncol <- 1
    
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric (z)
    z <- matrix (z [subscripts], ncol = ncol)
    x <- x [subscripts]
    y <- y [subscripts]
    minXwid <- if (length(unique(x)) > 1) 
        min(diff(sort(unique(x))))
    else 1
    minYwid <- if (length(unique(y)) > 1) 
        min(diff(sort(unique(y))))
    else 1
    fullZrange <- range(as.numeric(z), finite = TRUE)
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
    grid.rect(x = cx[idx], y = cy[idy], width = lx[idx] * scaleWidth(z, shrinkx[1], shrinkx[2], fullZrange), 
              height = ly[idy] * scaleWidth(z, shrinky[1], shrinky[2], fullZrange),
              default.units = "native",
              gp = gpar(fill = zcol, lwd = 1e-05, col = border))
}

panel.multivar <- function (x, y, z, subscripts = TRUE, at = pretty(z), 
  pch = 19, shift, ..., col.regions = regions$col) {
  
  ## fix subscripts
  if (is.matrix (z))
    subscripts <- unique (row (z) [subscripts])
  
  for (i in 1 : ncol (z)){
    if (scale.individual)
      at <- pretty (z)
    zcol <- level.colors (z [, i], at, col.regions [[i]], colors = TRUE)
    panel.xyplot(x [subscripts] + shift[i, 1], y [subscripts] + shift[i, 2],
                 col = zcol [subscripts], pch = pch, ...)
  }
}

panel.mixcol <- function (x, y, z, subscripts = TRUE, pch = 19, ...,
                                    col.regions = regions$col) {
  ## fix subscripts
  if (is.matrix (z))
    subscripts <- unique (row (z) [subscripts])
  
  zcol <- zcolors (z, col.regions, ...)

  panel.xyplot(x [subscripts], y [subscripts], col = zcol [subscripts], pch = pch, ...)
}

mixcol.legend <- function (purecol, data, labels = names (purecol), ...){
  top.vp <- viewport(layout = grid.layout(2, 2,
                 widths = unit(c(3.5, 1), c("lines", "null")),
                 heights = unit(c(1, 3), c("null", "lines"))))
  margin <- viewport(layout.pos.col = 1, layout.pos.row = 1,name = "margin")
  legend <- viewport(layout.pos.col = 2, layout.pos.row = 1,name = "legend")

  splot <- vpTree(top.vp, vpList(margin, legend))
  pushViewport(splot)

  seekViewport("margin")
  grid.text("fraction", x = unit(0.5, "lines"), rot = 90)

  if (missing (data))
    colmax <- rep (1, length (purecol))
  else
    colmax <- apply (data, 2, max)

  seekViewport("legend")
  pushViewport(dataViewport(0 : (length (purecol) + 1), c(0, max (colmax)), name = "plotlegend"))
  
  for (i in seq_along (purecol)) {
    cls <- colmix.rgb (seq (0, 1, length.out = 20), purecol [i])
    grid.rect (unit (rep (i, 20), "native"), unit (seq (0, colmax [i], length.out = 20), "native"),
               width = unit (1, "native"), height = unit (colmax [i] / 20, "native"),
               gp = gpar (fill = cls, col = cls))
    grid.text ( labels [i], unit (i, "native"), unit (0.5, "lines"), rot = -90, just = 0)
  }
 # grid.xaxis (at = seq_along (purecol), label=labels)
  grid.yaxis ()
}


myhist <- function (h, xlim, ylim, class, mix, purecol){
colmax <- apply (mix, 2, max)
mix <- sweep (mix, 2, apply (mix, 2, max), `/`)

  grid.newpage()

top.vp <- viewport(layout = grid.layout(3, 4,
                     widths = unit(c(4, 1, 3, 4), c("lines", "null", "lines", "lines")),
                     heights = unit(c(2, 1, 5), c("lines", "null", "lines"))))

margin1 <- viewport(layout.pos.col = 2, layout.pos.row = 3, name = "margin1")
margin2 <- viewport(layout.pos.col = 1, layout.pos.row = 2,name = "margin2")
margin3 <- viewport(layout.pos.col = 2, layout.pos.row = 1,name = "margin3")
margin4 <- viewport(layout.pos.col = 4, layout.pos.row = 2,name = "margin4")
margin4l <- viewport(layout.pos.col = 3, layout.pos.row = 2,name = "margin4l")
plot <- dataViewport(c(-4, 4), c(-2.5,5), layout.pos.col = 2, layout.pos.row = 2, name = "plot")

splot <- vpTree(top.vp, vpList(margin1, margin2,margin3, margin4,  plot, margin4l))

pushViewport(splot)
seekViewport("plot")

atx <- pretty (xlim, 7)
aty <- pretty (ylim, 7)

dx <- as.numeric (convertWidth (unit (1, "native"), "inches") ) 
dy <- as.numeric (convertHeight (unit (1, "native"), "inches")) 

cat ("adjust", dx / diff (xlim) / dy * diff (ylim), "\n")

if (dx / diff (xlim) > dy / diff (ylim)) {
  tmp <- dx / dy * diff (ylim) - diff (xlim)
  xlim <- xlim + c (-tmp, tmp) / 2
} else {
  tmp <- dy / dx * diff (xlim) - diff (ylim)
  ylim <- ylim + c (-tmp, tmp) / 2
}

pushViewport(dataViewport(xlim, ylim, name = "plotarea"))
grid.hexagons(h, style= "constant.col", border = "transparent", pen = colmix.rgb (mix, purecol))
grid.xaxis(at = atx, label = TRUE)
grid.yaxis(at = aty, label = TRUE)

seekViewport("margin1")
grid.text("LD 1", y = unit(1, "lines"))
seekViewport("margin2")
grid.text("LD 2", x = unit(1, "lines"), rot = 90)
seekViewport("margin4l")
grid.text("Counts", x = unit(0.5, "lines"), rot = 90)

seekViewport("margin4")
pushViewport(dataViewport(0:4, c(0, max (colmax)), name = "plotlegend"))
for (i in 1 : 3) {
  cls <- colmix.rgb (seq (0, 1, length.out = 20), purecol [i])
  grid.rect (unit (rep (i, 20), "native"), unit (seq (0, colmax [i], length.out = 20), "native"),
             width = unit (1, "native"), height = unit (colmax [i] / 20, "native"),
             gp = gpar (fill = cls, col = cls))
  grid.text ( class [i], unit (i, "native"), unit (0.5, "lines"), rot = 90, just = 1)
}
grid.yaxis ()

seekViewport("plotarea")
}

