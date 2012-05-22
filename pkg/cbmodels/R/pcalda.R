##'  LDA models using principle components as input space.
##'
##' @title PCA-LDA
##' @param X input variate matrix
##' @param Y matrix with class membership (classes correspond to columns).  If missing,
##' \code{\link[softclassval]{factor2matrix} (grouping)} is used.
##' @param grouping factor with class membership. If missing, \code{\link[softclassval]{hardclasses}
##' (Y)} is used.
##' @param ... ignored
##' @param na.rm 
##' @param comps which principal components should be used?
##' @return object of class "pcalda", consisting of the prcomp object returned by
##' \code{\link[stats]{prcomp}} and the lda object returned by \code{\link[MASS]{lda}}.
##' @author Claudia Beleites
##' @seealso \code{\link[stats]{prcomp}}, \code{\link[MASS]{lda}}
##' @export 
pcalda <- function (X, Y, grouping, ..., subset, na.rm = TRUE,
                    comps = stop ("principal components must be specified.")){
  
  tmp <- .ldapreproc <- function (X, Y, grouping, subset, na.rm)

  pca <- prcomp (X, center = tmp$center.x, scale = FALSE, subset = tmp$subset)
  
  lda <- lda (x = pca$x [, comps, drop = FALSE], grouping = tmp$grouping, subset = tmp$subset)

  structure (list (pca = pca,
                   lda = lda,
                   center.x = tmp$center.x,
                   subset = tmp$subset,
                   comps = comps),
             class = "pcalda")
}

predict.pcalda <- function (model, newdata, ...){
 # comps <- model$comps # otherwise predict.lda will choke!?
  
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
  colMeans (model$groupmeans)
}
