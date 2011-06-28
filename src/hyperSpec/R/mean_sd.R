##' Mean and Standard Deviation
##' Calculate mean and standard deviation, and mean, mean \eqn{\pm}{+-} one
##' standard deviation, respectively.
##' 
##' These functions are provided for convenience.
##' 
##' @aliases mean_sd
##' @rdname mean_sd
##' @param x a numeric vector
##' @param na.rm handed to \code{\link[base]{mean}} and \code{\link[stats]{sd}}
##' @return \code{mean_sd} returns a vector with two values (mean and standard
##'   deviation) of \code{x}.
##' @author C. Beleites
##' @seealso \code{\link[base]{mean}}, \code{\link[stats]{sd}}
##' @keywords univar
##' @export
##' @examples
##' 
##'   mean_sd (flu$c)
 
mean_sd <- function (x, na.rm = TRUE)
  c(mean = mean (x, na.rm = na.rm),
    sd = sd (x, na.rm = na.rm)
    )

##' @aliases mean_pm_sd
##' @rdname mean_sd
##' @return
##'  
##' \code{mean_pm_sd} returns a vector with 3 values: mean - 1 sd, mean, mean + 1 sd.
##' @export
##' @examples
##' 
##'   mean_pm_sd (flu$c)
##' 

mean_pm_sd <- function (x, na.rm = TRUE){
  m <- mean (x, na.rm = na.rm)
  s <- sd (x, na.rm = na.rm)
  c("mean.minus.sd" = m - s, "mean" = m, "mean.plus.sd" = m + s)
}

