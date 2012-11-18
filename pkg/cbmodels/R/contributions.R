##' Contributions of Variates to Model Predictions
##'
##' Linear models procduce scores according to
##' \deqn{S = (X - center) \times coefficients)}{S = (X - center) x coefficients}
##' from the data matrix X, the center of the model and the model coefficients (scaling, rotation,
##' loadings, latent variables, ...)
##'
##' To study the behaviour of the model it is often useful to calculate the element-wise product of
##' centered data and the for each latent variable (component, discriminant function, ...). This is
##' done by \code{contributions}
##' @title contriubutions
##' @param object a model
##' @param newdata data to use for calculating contributions
##' @param dims dimensions in model space of which the contributions should be calculated
##' @param ... further parameters
##' @return an array of dimension (\code{nrow (newdata)} x \code{ncol (newdata)} x \code{length
##' (dims)})
##' @author Claudia Beleites
##' @export
contributions <- function (object, newdata, dims = TRUE, ...) UseMethod ("contributions")

##' @noRd
contributions.default <- function (object, newdata, dims, ...){
  stop ("contributions is not implemented for class '", class (object), "'.")
}

.test (contributions.default) <- function (){
  checkException (contributions (NULL)) ## not defined
}

## workhorse function
.contributions <- function (x, center, coef, dims){
  
  x <- scale(x, center=center, scale=FALSE)
  coef <- 1 / coef [, dims, drop = FALSE] # use 1/coef so later on `scale` can be used for
                                           # multiplication
  
  contrib <- array (NA, c (dim (x), ncol (coef)))
  dimnames (contrib) <- c (dimnames (x), dimnames (coef)[2L])
  names (dimnames (contrib)) <- c ("row", "variate", "LD")

  for (p in seq_len (ncol (coef)))
      contrib [,, p] <- scale (x, center = FALSE, scale = coef [, p])
  
  contrib
}
