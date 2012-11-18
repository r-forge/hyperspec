##' @seealso 
##' \code{\link[cbmodels]{pcalda}} 
##' @export
##' @include contributions.R
##' @method contributions pcalda
##' @S3method contributions pcalda
##' @rdname contributions
##' @examples
##' 1+1
contributions.pcalda <- function (object, newdata = stop ("newdata is required: ",
                                            "pcalda models do not store the original data"),
                                  dims = TRUE, ...){
  if (length (c (...)) > 0L)
      warning ("additional arguments ignored: ", paste (..., collapse = ", "), ".")

  .contributions (x = newdata,
                  center = center (object),
                  coef = coef (object) [, dims, drop = FALSE])
}

.test (contributions.pcalda) <- function () {
  X <- as.matrix (iris [, -5])
  model <- pcalda (X = X, grouping = iris [,5], comps = 1:3)
  pred <- predict (model)$x

  contributions <- contributions (model, X)

  checkEqualsNumeric (apply (contributions, c (1, 3), sum), pred)
  checkEqualsNumeric (contributions [,,2], contributions (model, X, dims = 2))
  checkEqualsNumeric (contributions [,,2], contributions (model, X, dims = -1))
  checkEqualsNumeric (contributions [1:10,,2], contributions (model, X [1:10,], dims = -1))

  checkException (contributions (model)) # PCA-LDA models do not store the original data
}

