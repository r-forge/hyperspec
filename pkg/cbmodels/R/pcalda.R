##'  LDA models using principle components as input space.
##'
##' @title PCA-LDA
##' @param X input variate matrix
##' @param Y matrix with class membership (classes correspond to columns).  If missing,
##' \code{\link[softclassval]{factor2matrix} (grouping)} is used.
##' @param grouping factor with class membership. If missing, \code{\link[softclassval]{hardclasses}
##' (Y)} is used.
##' @param ... handed to \code{\link[stats]{prcomp}} and \code{\link[MASS]{lda}} (e.g. \code{subset})
## ' @param na.action see \code{\link{scale}}, \code{\link[stats]{prcomp}}, and
##' \code{\link[MASS]{lda}}.
##' @param comps which principal components should be used?
##' @return object of class "pcalda", consisting of the prcomp object returned by
##' \code{\link[stats]{prcomp}} and the lda object returned by \code{\link[MASS]{lda}}.
##' @author Claudia Beleites
##' @seealso \code{\link[stats]{prcomp}}, \code{\link[MASS]{lda}}
##' @export
##' @include cbmodels.R
##' @include center.R
##' @examples
##' chondro <- chondro [! is.na (chondro$clusters)]
##' model <- pcalda (X = chondro[[]], grouping = chondro$clusters, comps = 1 : 3)
##' 
##' names (model)
##' 
##' pred <- predict (model)
##'
##' plot
pcalda <- function (X, Y, grouping, comps = TRUE, ...#,
                    #subset = TRUE, na.action = na.exclude
                    ){
  
  tmp <- .ldapreproc (X, Y, grouping)

  pca <- prcomp (tmp$X, center = tmp$center.x, scale = FALSE, ...)
  
  lda <- lda (x = pca$x [, comps, drop = FALSE], grouping = tmp$grouping, ...)

  structure (list (pca = pca,
                   lda = lda,
                   comps = comps),
             class = "pcalda")
}


##' @param object the PCA-LDA model
##' @param newdata the new data to apply the model to (matrix)
##' @param ... \code{predict} ignores further arguments
##' @rdname pcalda
##' @export
predict.pcalda <- function (object, newdata, ...){
 
  if (missing (newdata)){
    pcascores <- object$pca$x [, object$comps, drop = FALSE]
  } else {
    pcascores <- scale (newdata, center = object$pca$center, scale = FALSE) %*%
      object$pca$rotation [, object$comps, drop = FALSE]
  }

  res <- predict (object$lda, newdata = pcascores)
  res$pcascores <- pcascores
  res$comps = object$comps

  res
}

##' @rdname pcalda
##' @export
coef.pcalda <- function (object, ...){
  object$pca$rotation [, object$comps, drop = FALSE] %*% object$lda$scaling
}

##' @rdname pcalda
##' @export
center.pcalda <- function (object, ...){
  object$pca$center
}


.test (pcalda) <- function (){
  X <- as.matrix (iris [,c ("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
  grp <- iris$Species

  complist <- list (TRUE, 1:3, 4)

  for (comps in complist) {
    model <- pcalda (X = X, grouping = grp, comps = comps)

    ## x-centering of the model
    checkEqualsNumeric (colMeans (model$lda$means), rep (0, nrow (model$lda$scaling)),
                        msg = sprintf ("centering with comps = %s", comps))

    ## center (model)
    checkEqualsNumeric (model$pca$center, center (model),
                        msg = sprintf ("center (model); %s comps", comps))
    coef <- model$pca$rotation [, comps, drop = FALSE] %*% coef (model$lda)
    checkEqualsNumeric (coef (model), coef,
                        msg = sprintf ("coefficients with comps = %s", comps))

    grpmeans <- aggregate (X, by = list (grp), mean) [-1]
    scores <- scale (X, center = colMeans (grpmeans), scale = FALSE) %*% coef
    checkEqualsNumeric (predict (model)$x, scores,
                        msg = sprintf ("scores with comps = %s", comps))
  }
    
  ## test na.action

  ## test center
  
}
