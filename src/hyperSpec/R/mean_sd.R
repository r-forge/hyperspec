##' Mean and Standard Deviation
##' Calculate mean and standard deviation, and mean, mean \eqn{\pm}{+-} one
##' standard deviation, respectively.
##' 
##' These functions are provided for convenience.
##' 
##' @rdname mean_sd
##' @param x a numeric vector
##' @param na.rm handed to \code{\link[base]{mean}} and \code{\link[stats]{sd}}
##' @param \dots ignored (needed to make function generic)
##' @return \code{mean_sd} returns a vector with two values (mean and standard
##'   deviation) of \code{x}.
##' @seealso \code{\link[base]{mean}}, \code{\link[stats]{sd}}
##' @keywords multivar
##' @export
##' @examples
##' 
##' mean_sd (flu [,, 405 ~ 410])
 
mean_sd <- function (x, na.rm = TRUE, ...)
  c (mean = mean (x, na.rm = na.rm),
     sd   = sd   (x, na.rm = na.rm)
    )

##' @rdname mean_sd
##' @param short,user,date handed to \code{\link{logentry}}.
##' @return \code{mean_sd} returns a hyperSpec object with the mean spectrum in the first row and the standard deviation in the 2nd.
##' @author C. Beleites
##' @seealso \code{\link[base]{mean}}, \code{\link[stats]{sd}}
##' @keywords univar
##' @export
##' @examples
##' 
##' mean_sd (flu)
setMethod ("mean_sd", signature = signature (x = "hyperSpec"),
           function (x, na.rm = TRUE, ..., short = "mean_sd", user = NULL, date = NULL) {
             m <- colMeans (x@data$spc)
             s <- sd       (x@data$spc)
             decomposition (x, rbind (mean = m, sd = s), scores = FALSE,
                            short = short, user = user, date = date)
           })



##' @aliases mean_pm_sd
##' @rdname mean_sd
##' @return
##'  
##' \code{mean_pm_sd} returns a vector with 3 values: mean - 1 sd, mean, mean + 1 sd
##' @export
##' @examples
##' 
##'   mean_pm_sd (flu$c)
mean_pm_sd <- function (x, na.rm = TRUE, ...){
  m <- mean (x, na.rm = na.rm)
  s <- sd (x, na.rm = na.rm)
  c("mean.minus.sd" = m - s, "mean" = m, "mean.plus.sd" = m + s)
}

##' @rdname mean_sd
##' @return For hyperSpec object, \code{mean_pm_sd} returns a hyperSpec object containing mean - sd,
##' mean, and mean + sd spectra.
##' @author C. Beleites
##' @seealso \code{\link[base]{mean}}, \code{\link[stats]{sd}}
##' @keywords univar
##' @export
##' @examples
##' 
##' mean_pm_sd (flu)
setMethod ("mean_pm_sd", signature = signature (x = "hyperSpec"),
           function (x, na.rm = TRUE, ..., short = "mean_sd", user = NULL, date = NULL) {
             m <- colMeans (x@data$spc)
             s <- sd       (x@data$spc)
             decomposition (x, rbind ("mean - sd" = m - s, mean = m, "mean + sd"= m + s), 
                            short = short, user = user, date = date)
           })
