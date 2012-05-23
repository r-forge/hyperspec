##'  LDA models using principle components as input space.
##'
##' @title PCA-LDA
##' @param X input variate matrix
##' @param Y matrix with class membership (classes correspond to columns).  If missing,
##' \code{\link[softclassval]{factor2matrix} (grouping)} is used.
##' @param grouping factor with class membership. If missing, \code{\link[softclassval]{hardclasses}
##' (Y)} is used.
##' @param ... ignored
##' @param na.action see \code{\link{scale}}, \code{\link[stats]{prcomp}}, and
##' \code{\link[MASS]{lda}}.
##' @param comps which principal components should be used?
##' @return object of class "pcalda", consisting of the prcomp object returned by
##' \code{\link[stats]{prcomp}} and the lda object returned by \code{\link[MASS]{lda}}.
##' @author Claudia Beleites
##' @seealso \code{\link[stats]{prcomp}}, \code{\link[MASS]{lda}}
##' @export 
pcalda <- function (X, Y, grouping, comps = TRUE, ...,
                    subset = TRUE, na.action = na.exclude){
  
  tmp <- .ldapreproc (X, Y, grouping, subset, na.action)

  pca <- prcomp (tmp$X, center = tmp$center.x, scale = FALSE, subset = tmp$subset)
  
  lda <- lda (x = pca$x [, comps, drop = FALSE], grouping = tmp$grouping, subset = tmp$subset)

  structure (list (pca = pca,
                   lda = lda,
  #                 center.x = tmp$center.x, not needed: is in pca$center
                   subset = tmp$subset,
                   comps = comps),
             class = "pcalda")
}


predict.pcalda <- function (model, newdata, ...){
 
  if (missing (newdata)){
    pcascores <- model$pca$x [, model$comps, drop = FALSE]
  } else {
    pcascores <- scale (newdata, center = model$pca$center, scale = FALSE) %*%
      model$pca$rotation [, model$comps, drop = FALSE]
  }

  res <- predict (model$lda, newdata = pcascores)
  res$pcascores <- pcascores
  res$comps = model$comps

  res
}

coef.pcalda <- function (model){
  model$pca$rotation [, model$comps, drop = FALSE] %*% model$lda$scaling
}

center.pcalda <- function (model){
  colMeans (model$center.x)
}


.test (pcalda) <- function (){
  X <- iris [,c ("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
  grp <- iris$Species

  complist <- list (TRUE, 1:3, 4)

  for (comps in complist) {
    model <- pcalda (X = X, grouping = grp, comps = comps)

    checkEqualsNumeric (colMeans (model$lda$means), rep (0, nrow (model$lda$scaling)),
                        msg = sprintf ("centering with comps = %s", comps))

    coef <- model$pca$rotation [, comps] %*% coef (model$lda)
    checkEqualsNumeric (coef (model), coef,
                        msg = sprintf ("coefficients with comps = %s", comps))

    grpmeans <- aggregate (X, by = list (grp), mean) [-1]
    scores <- scale (X, center = colMeans (grpmeans), scale = FALSE) %*% coef
    checkEqualsNumeric (predict (model)$x, scores,
                        msg = sprintf ("scores with comps = %s", comps))
  }
    
  ## test na.action

  
}
