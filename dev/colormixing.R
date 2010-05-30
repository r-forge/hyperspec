library (plotrix)
library (hyperSpec)
library (colorspace)

n <- 25
mix <- expand.grid (seq (0, 1, length.out = n),
                    seq (0, 1, length.out = n),
                    seq (0, 1, length.out = n))
mix <- mix [round (rowSums (mix), digits=2) == 1,]
mix <- as.matrix (mix)

## naive: color just as written in channel
triax.plot (mix, col.symbols = rgb (mix), pch = 20)
## i.e. additive mixing. => mixes against black

#this mixes against white (subtractive), but inverse colors
triax.plot (mix, col.symbols = rgb (1 - mix), pch = 20)

# other colors:
purecol <- matrix (c(1,   0,    0,
                     0, 0.5,    0,
                     0,   0, 0.8), ncol = 3, byrow = TRUE)

triax.plot (mix, col.symbols = rgb (mix %*% purecol), pch = 19)

# mix these against white:
# additive
triax.plot (mix, col.symbols = rgb (mix %*% purecol), pch = 19)

#subtractive
triax.plot (mix, col.symbols = rgb (1 - mix %*% (1 - purecol)), pch = 19)
triax.plot (mix, col.symbols = rgb (1 - mix^2 %*% (1 - purecol)), pch = 19)
## doesn't matter here

## enhancing function
enh <- function (x){
  x <-  1.4 * x - .4
  x [x > 1] <- 1
  x [x < 0] <- 0
  x
}

triax.plot (mix, col.symbols = colmix.rgb (cbind (mix [, 1], 0, 0),
                   purecol, against = 0.80), pch = 19)
triax.plot (mix, col.symbols = colmix.rgb (enh (mix), purecol), pch = 19)
triax.points (t(c(1, 1, 1)/3))


blues <-  rev (sequential_hcl(20))# (h = 240, c = 0:100, l = 20))
reds <-  rev (sequential_hcl (20, h = 0, c = c(130, 0), l = c (40, 95)))
greens <-  rev (sequential_hcl (20, h = 120, c = c(130, 0), l = c (40, 95)))

df <- expand.grid (x = seq (-10, 10, length.out = 50),
                   y = seq (-10, 10, length.out = 50))
z <-  matrix(c (dnorm (df$x, -5) * dnorm (df$y, -5),
                dnorm (df$x, 3) * dnorm (df$y, 2),
                dnorm (df$x, 1) * dnorm (df$y, 3)
                ), ncol = 3)

library (grid)
levelplot (z ~ x * y, df, panel = panel.mixlevelplot,
           col.regions = c("#000080", "#FF0000","#008000"),
           aspect = "iso")
levelplot (z ~ x * y, df, panel = panel.mixcol, #
           col.regions = c("#000080", "#FF0000","#008000"),
           pch = 20, cex = 3, aspect = "iso")

levelplot (z ~ x * y, df, col.regions = c (c("#FF0000", "#00B000", "#0000B0")),
           panel = function (x, y, z, subscripts, ...) {
             panel.mixlevelplot (x, y, z, subscripts, ...)
             subscripts <- unique (row (z) [subscripts])
             for (i in 1 : ncol (z))
               panel.levelplot (x, y, z [, i], subscripts = subscripts, contour = TRUE, region = FALSE)
           }
           )

## now for the desc lda

library (MASS)
load ("/home/cb/Uni/Diss/Auswertung/2010-04-28-lymph/lymph.RData")
crisp <- apply (lymph$lymph == 1, 1, any)
lda <- lda (lymphlda ~ spc, lymph[crisp]$.)
desclda <- predict (lda, lymph$.)

nbin = 300; xbnds = c (-35, 35); ybnds = c (-35, 35)

h <- hexbin (desclda$x [iincl ,1], desclda$x [, 2], nbin,
             xbnds = xbnds, ybnds = ybnds)

hib <- hexbin (desclda$x[!is.na (lymph$lymphlda) & lymph$lymphlda == "ib", 1],
               desclda$x[!is.na (lymph$lymphlda) & lymph$lymphlda == "ib", 2],
               nbin, xbnds = xbnds, ybnds = ybnds)
haa <- hexbin (desclda$x[!is.na (lymph$lymphlda) & lymph$lymphlda == "aa", 1],
               desclda$x[!is.na (lymph$lymphlda) & lymph$lymphlda == "aa", 2],
               nbin, xbnds = xbnds, ybnds = ybnds)
hdaaac <- hexbin (desclda$x[!is.na (lymph$lymphlda) & lymph$lymphlda == "daaac", 1],
                  desclda$x[!is.na (lymph$lymphlda) & lymph$lymphlda == "daaac", 2],
                  nbin, xbnds = xbnds, ybnds = ybnds)
dfl <- data.frame (hcell2xy (h), h = h@count)
dfl$order <- 1 : nrow (dfl)
dfl <- merge (dfl, data.frame (hcell2xy (hib), hib = hib@count), all.x = TRUE)
dfl <- merge (dfl, data.frame (hcell2xy (haa), haa = haa@count), all.x = TRUE)
dfl <- merge (dfl, data.frame (hcell2xy (hdaaac), hdaaac = hdaaac@count), all.x = TRUE)
dfl [is.na (dfl)] <- 0

mix <- as.matrix (dfl [, c ("hib", "haa", "hdaaac")])
mix <- sweep (mix, 2, apply (mix, 2, max), `/`)

dev.size ()

pdf ("/home/cb/Uni/Diss/DissTEX/Zeichnungen/lymph-descLDA.pdf", width = 8, height = 6.5)
myhist (h, xlim = c (-4, 4), ylim = c (-2.5, 5),
        class = c ("N", "A", "L"),
        purecol = c ("darkgreen","orange", "#800080"),
        mix = as.matrix (dfl [, c ("hib", "haa", "hdaaac")])[order (dfl$order), ])
for (i in 1 : 3) {
  ispc <- ! is.na (lymph$lymphlda) & as.numeric (lymph$lymphlda) == i 
  shape <- var (desclda$x[ispc, 1 : 2])
  center <- colMeans (desclda$x[ispc, 1 : 2])
  e <- ellipse (shape, centre = center, level = .5)
  grid.lines (e[, 1], e[, 2], default.units = "native")
  print (sum(as.logical (point.in.polygon (desclda$x[ispc, 1],
                                           desclda$x[ispc, 2], e[,1], e [,2]))) / sum (ispc))
}
dev.off ()


crisp <- apply (lymph$astro3 == 1, 1, any)
lda <- lda (astro3lda ~ spc, lymph[crisp]$.)
desclda <- predict (lda, lymph$.)

nbin = 300; xbnds = c (-35, 35); ybnds = c (-35, 35)

h <- hexbin (desclda$x [, 1], desclda$x [, 2], nbin,
             xbnds = xbnds, ybnds = ybnds)

hib <- hexbin (desclda$x[!is.na (lymph$astro3lda) & lymph$astro3lda == 1, 1],
               desclda$x[!is.na (lymph$astro3lda) & lymph$astro3lda == 1, 2],
               nbin, xbnds = xbnds, ybnds = ybnds)

erodebin <- erode.hexbin(hib, cdfcut=.5)

haaa <- hexbin (desclda$x[!is.na (lymph$astro3lda) & lymph$astro3lda == 2, 1],
               desclda$x[!is.na (lymph$astro3lda) & lymph$astro3lda == 2, 2],
               nbin, xbnds = xbnds, ybnds = ybnds)
haab <- hexbin (desclda$x[!is.na (lymph$astro3lda) & lymph$astro3lda == 3, 1],
                  desclda$x[!is.na (lymph$astro3lda) & lymph$astro3lda == 3, 2],
                  nbin, xbnds = xbnds, ybnds = ybnds)
dfa <- data.frame (hcell2xy (h), h = h@count)
dfa$order <- 1 : nrow (dfa)
dfa <- merge (dfa, data.frame (hcell2xy (hib), hib = hib@count), all.x = TRUE)
dfa <- merge (dfa, data.frame (hcell2xy (haaa), haaa = haaa@count), all.x = TRUE)
dfa <- merge (dfa, data.frame (hcell2xy (haab), haab = haab@count), all.x = TRUE)
dfa [is.na (dfa)] <- 0

mix <- as.matrix (dfa [, c ("hib", "haaa", "haab")])
mix <- sweep (mix, 2, apply (mix, 2, max), `/`
)
dev.size ()
library (sp)
library (ellipse)
pdf ("/home/cb/Uni/Diss/DissTEX/Zeichnungen/astro-descLDA.pdf", width = 8, height = 6.5)
myhist (h, xlim = c (-4, 4), ylim = c (-3.5, 3),
        class = c ("N", "A°II", "A°III +"),
        purecol = c ("#009000","#0000B0", "#D00000"),
        mix =   as.matrix (dfa [, c ("hib", "haaa", "haab")]) [order (dfa$order), ])

for (i in 1 : 3) {
  ispc <- ! is.na (lymph$astro3lda) & lymph$astro3lda == i 
  shape <- var (desclda$x[ispc, 1 : 2])
  center <- colMeans (desclda$x[ispc, 1 : 2])
  e <- ellipse (shape, centre = center, level = .5)
  grid.lines (e[, 1], e[, 2], default.units = "native")
  print (sum(as.logical (point.in.polygon (desclda$x[ispc, 1], desclda$x[ispc, 2], e[,1], e [,2]))) / sum (ispc))
}

dev.off ()

