#################################################################################
###
###  txt.R - read .txt file with samples in columsn
###  Time-stamp: <Claudia Beleites on Tuesday, 2010-02-09 at 13:57:52 on cb>
###  
###  
###  Version 1.0  2010-02-06 12:34  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

library (hyperSpec)

file <- read.table ("Triazine 5_31.txt", header = TRUE, dec = ",", sep = "\t")
dim (file)
summary (file)

triazine <- new ("hyperSpec", wavelength = file [,1],
                 spc = t (file [, -1]),
                 label = list (.wavelength = "angle / °", spc = "I / a.u."))

plot (triazine)

plot (triazine, "mat", col.regions = matlab.palette (20))
triazine <- new ("hyperSpec", wavelength = file [,1],
                 spc = t (file [, -1]),
                 data = data.frame (sample = colnames (file) [-1]),
                 label = list (.wavelength = "angle / °", spc = "I / a.u."))
triazine

interpolate.spc <- function (a, wl, sintheta, ref.spc, new.spc) {
  new.wl <- wl + a[1] + a[2] * sintheta
  tmp <- approx (x = wl, y = new.spc, xout = new.wl)
}
  

spc.match <- function (a, wl, sintheta, ref.spc, new.spc) {
  tmp <- interpolate.spc (a, wl, sintheta, ref.spc, new.spc)$y

  keep <- !is.na (tmp)  

  - sum (tmp [keep] * ref.spc [keep])
#   - cor (tmp [keep], ref.spc [keep])
}

reference <- triazine [1]
new.spc <- triazine [6]
plot (reference)
plot (new.spc, add = TRUE, col = "blue")

plot(reference - new.spc)

opt <- optim (par = c (0, 0), spc.match,
              wl = wl (reference),
              sintheta = sin (wl (reference)*pi/360),
              ref.spc = as.vector (reference [[]]),
              new.spc = as.vector (new.spc [[]])
              )
opt

interp.spc <- new.spc
interp.spc [[]] <- interpolate.spc (opt$par,
                                    wl = wl (reference),
                                    sintheta = sin (wl (reference)*pi/360),
                                    ref.spc = as.vector (reference [[]]),
                                    new.spc = as.vector (new.spc [[]]))$y
plot (reference)
plot (new.spc, add = TRUE, col = "blue", lines.args = list (lty = 3))
plot (interp.spc, add = TRUE, col = "blue")

plot(reference - new.spc)
plot(reference - interp.spc, add = TRUE, col = "red")

plot (reference [,,24 ~ 30])
plot (new.spc, add = TRUE, col = "blue", lines.args = list (lty = 3))
plot (interp.spc, add = TRUE, col = "blue")


## you need to check for convergence, though
value <- numeric (250)
fit <- matrix (NA, 250, 2)

for (i in 1 : 250 ) {
  opt <- optim (par = runif(2) * 2 - 1, spc.match,
                wl = wl (reference),
                sintheta = sin (wl (reference)*pi/360),
                ref.spc = as.vector (reference [[]]),
                new.spc = as.vector (new.spc [[]])
                )
  value [i] <- opt$value
  fit [i,] <- opt$par
}

hist (value)
hist (value [value < -.5])

summary (fit [value < -.5, ])

hist (fit [value < -.5, 1])
hist (fit [value < -.5, 2])

df <- data.frame (fit, value)
plot (df$X1, df$X2, col = rev (matlab.palette(20, alpha = .1)) [cut (value, 20)], pch = 20)

# you can also manually look for start values
a0 <- rep (seq (-1, 1, length.out = 25), each = 25)
a1 <- rep (seq (-1, 1, length.out = 25), 25)

df2 <- data.frame (a0, a1)
val <- apply (df2, 1, spc.match,
              wl = wl (reference),
              sintheta = sin (wl (reference)*pi/360),
              ref.spc = as.vector (reference [[]]),
              new.spc = as.vector (new.spc [[]])
              )
df2$val <- val

levelplot (val ~ a0 * a1, df2, contour = T)

a0 <- rep (seq (-0.1, 0.1, length.out = 25), each = 25)
a1 <- rep (seq (-1, 1, length.out = 25), 25)

df2 <- data.frame (a0, a1)
val <- apply (df2, 1, spc.match,
              wl = wl (reference),
              sintheta = sin (wl (reference)*pi/360),
              ref.spc = as.vector (reference [[]]),
              new.spc = as.vector (new.spc [[]])
              )
df2$val <- val

levelplot (val ~ a0 * a1, df2, contour = T)

opt


lorentz <- function (x, x0, gamma) gamma / ((x - x0)^2 + gamma^2/4) / 2 / pi
gauss <- 

plot (lorentz (seq (0, 1, length.out = 100), 0.55, .5), type = "b")

spc <- triazine [[6,,26.2 ~ 27, drop = T]]



optim (par = c (25, 3),
       function (x) sum ((spc - lorentz (seq_along (spc), x [1], x [2]))^2)
       )

lines (lorentz (seq_along (spc), 25.950, 3.068))


mean (triazine [[6,,26.2 ~ 27]])



smooth <- spc.loess (triazine,  newx = seq (26.2, 27, length.out = 100))

peakpos <- apply (smooth [[]], 1, which.max)
peakpos <- i2wl (smooth, peakpos)
peakpos
plot (sort (peakpos))

shift <- 26.6 - peakpos # 

shifted <- triazine
for (i in seq (shifted, index = TRUE))
  shifted [[i]] <- approx (wl (shifted) + shift [i], y = shifted [[i]], xout = wl (shifted))$y


plot (triazine [,,25 ~ 30], plot.args = list (ylim = c(0, 2)))
plot (shifted [,,25 ~ 30],  add = TRUE, yoffset = 1)

