##' doMC version of spc.loess using foreach - Simon Fuller 12/6/2012
##' loess smoothing interpolation for spectra
##' Spectra can be smoothed and interpolated on a new wavelength axis using
##' \code{\link[stats]{loess}}.
##' 
##' Applying \code{\link[stats]{loess}} to each of the spectra, an interpolation onto a new
##' wavelength axis is performed.  At the same time, the specta are smoothed in order to increase the
##' signal : noise ratio. See \code{\link[stats]{loess}} and \code{\link[stats]{loess.control}} on
##' the parameters that control the amount of smoothing.
##' 
##' @param spc the \code{hyperSpec} object
##' @param newx wavelengh axis tointrpolate on
##' @param enp.target,surface,\dots parameters for \code{\link[stats]{loess}} and
##' \code{\link[stats]{loess.control}}. 
##' @param short,user,date handed to \code{\link{logentry}}.
##' @return a new \code{hyperspec} object.
##' @rdname spc-loess
##' @export
##' @author C. Beleites
##' @seealso \code{\link[stats]{loess}}, \code{\link[stats]{loess.control}}
##' @keywords manip datagen
##' @examples
##' 
##' plot (flu, col = "darkgray")
##' plot (spc.loess(flu, seq (420, 470, 5)), add = TRUE, col = "red")
##' 
##' flu [[3, ]] <- NA_real_
##' smooth <- spc.loess(flu, seq (420, 470, 5))
##' smooth [[, ]]
##' plot (smooth, add = TRUE, col = "blue")
##' 
par.spc.loess <- function (spc, newx, enp.target = nwl (spc) / 4,
                       surface = "direct", ...,
                       short = "spc.loess", user = NULL, date = NULL){

  .loess <- function (y, x){
    if (all (is.na (y)))
      NA
    else
      loess (y ~ x, enp.target = enp.target, surface = surface, ...)
  }

  .predict <-  function (loess, x){
    if (!is (loess, "loess") && is.na (loess))
      rep (NA_real_, length (x))
    else
      predict (loess, x)
  }


	require("doMC")#add
	registerDoMC()#add  

  chk.hy (spc)
  validObject (spc)

  if (any (newx < min (spc@wavelength)) || any (newx > max (spc@wavelength)))
    warning ("newx outside spectral range of spc. NAs will be generated.")

	len <- nrow (spc[[]]) 
	
	loess.ob <- foreach (i=1:len) %dopar% {
		.predict(.loess (spc@data$spc[i,], spc@wavelength),newx)
	}
	#spc@data$spc <- t (sapply (loess.ob, .predict, newx))

	#spc@data$spc <- foreach (i=1:len, .combine=rbind) %dopar% {
	#	.predict( loess.ob[[i]], newx )
	#}
  
   .wl(spc) <- newx

  .logentry (spc, short =  short,
             long = list (newx = newx, enp.target = enp.target,
               surface = surface, ...),
             user = user, date = date)
}
