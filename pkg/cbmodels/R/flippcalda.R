##' @seealso 
##' \code{\link[cbmodels]{pcalda}} 
##' @export
##' @include contributions.R
##' @include contributionslda.R
##' @method contributions pcalda
##' @S3method contributions pcalda
##' @rdname contributions
##' @examples
##'
contributions.pcalda <- function (object, newdata = stop ("newdata is required: ",
                                            "pcalda models do not store the original data"),
                                  dimen = TRUE){
  .contributions (x = newdata,
                  center = center (object),
                  coef = coef (object) [, dimen, drop = FALSE])
}

.test (contributions.pcalda) <- function () {
  X <- as.matrix (iris [, -5])
  model <- pcalda (X = X, grouping = iris [,5], comps = 1:3)
  pred <- predict (model)$x

  contributions <- contributions (model, X)

  checkEqualsNumeric (apply (contributions, c (1, 3), sum), pred)
  checkEqualsNumeric (contributions [,,2], contributions (model, X, dimen = 2))
  checkEqualsNumeric (contributions [,,2], contributions (model, X, dimen = -1))
  checkEqualsNumeric (contributions [1:10,,2], contributions (model, X [1:10,], dimen = -1))

  checkException (contributions (model)) # PCA-LDA models do not store the original data
}

