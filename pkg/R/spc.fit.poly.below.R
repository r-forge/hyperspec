###-----------------------------------------------------------------------------
###
###  spc.fit.poly.below
###
###

spc.fit.poly.below <- function (fit.to, apply.to = fit.to, poly.order = 1,
                                npts.min = NULL, noise = 0,
                                short = "spc.fit.poly.below", user = NULL,
                                date = NULL){
  .is.hy (fit.to)
  if (! is.null (apply.to))
    .is.hy (apply.to)

  validObject (fit.to)
  validObject (apply.to)

  if (is.null (npts.min)){
    npts.min <- max (round (nwl(fit.to) * 0.05), 3 * (poly.order + 1))
    cat ("Fitting with npts.min = ",  npts.min, "\n")
  } else  if (npts.min <= poly.order){
    npts.min <- poly.order + 1
    warning (paste ("npts.min too small: adjusted to", npts.min))
  }

  if (length (noise) == 1)
    noise <- rep (noise, nrow (fit.to))

  vdm <- outer(fit.to@wavelength, 0 : poly.order, "^")
  y <- t(fit.to [[]])

  p <- matrix (nrow = nrow(fit.to) , ncol = poly.order + 1)
  for (i in row.seq (fit.to)){
    use.old <- logical (nwl (fit.to))
    use <- !use.old

    repeat {
      p[i,] <- qr.solve (vdm[use,], y[use, i])
      bl <- vdm %*% p [i,]
      use.old <- use
      use <- y[, i] < bl + noise [i]
      if ((sum (use) < npts.min) || all (use == use.old))
        break
    }
  }
  if (is.null (apply.to)){
    fit.to@data$spc <- p
    .wl (fit.to) <- 0 : poly.order
    colnames (fit.to@data$spc) <- paste ("x^", 0 : poly.order, sep="")

    validObject (fit.to)
    .logentry (fit.to, short = short,
               long = list (apply = NULL, poly.order = poly.order,
                 npts.min = npts.min, noise = noise),
               user = user, date = date)
  } else {
    x <- apply.to@wavelength

    vdm <- outer(x, 0 : poly.order, "^")             # Vandermonde matrix of x

    apply.to@data$spc <- I (t (apply (p, 1, function (p, x) {x %*% p}, vdm)))

    .wl(apply.to) <- x
    colnames (apply.to@data$spc) <- format (x, digits = 4)

    validObject (apply.to)
    .logentry (apply.to, short = short,
               long = list (apply = match.call()$apply, poly.order = poly.order,
                 npts.min = npts.min, noise = noise),
               user = user, date = date)
  }
}
