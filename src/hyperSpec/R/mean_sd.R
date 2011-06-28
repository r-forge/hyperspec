###-----------------------------------------------------------------------------
###
### mean_sd
###
###



##’ Mean and Standard Deviation
##’ Calculate mean and standard deviation, and mean, mean \eqn{\pm}{+-} one
##’ standard deviation, respectively.
##’ 
##’ These functions are provided for convenience.
##’ 
##’ @aliases mean_sd mean_pm_sd
##’ @param x a numeric vector
##’ @param na.rm handed to \code{\link[base]{mean}} and \code{\link[stats]{sd}}
##’ @return \code{mean_sd} returns a vector with two values (mean and standard
##’   deviation) of \code{x}.
##’ 
##’ \code{mean_pm_sd} returns a vector with 3 values: mean - 1 sd, mean, mean +
##’   1 sd.
##’ @author C. Beleites
##’ @seealso \code{\link[base]{mean}}, \code{\link[stats]{sd}}
##’ @keywords univar
##’ @examples
##’ 
##’   mean_sd (flu$c)
##’   mean_pm_sd (flu$c)
##’ 
mean_sd <- function (x, na.rm = TRUE)
  c(mean = mean (x, na.rm = na.rm),
    sd = sd (x, na.rm = na.rm)
    )

