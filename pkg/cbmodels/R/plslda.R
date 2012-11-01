##' LDA models using PLS latent variables as input space.
##'
##' For the moment only \code{\link{kernelpls.fit}} (see \code{\link[pls]{kernelpls.fit}} for the
##' original) is supported.
##' 
##' @title PLS-LDA
##' @param X input variate matrix
##' @param Y matrix with class membership (classes correspond to columns).  If missing,
##' \code{\link[softclassval]{factor2matrix} (grouping)} is used.
##' @param grouping factor with class membership. If missing, \code{\link[softclassval]{hardclasses}
##' (Y)} is used.
##' @param comps which latent variables should be used?
##' @param ncomp how many latent variables should be calculated?
##' @param stripped should the model be stripped to save memory? (Stripping is different from \code{\link[pls]{plsr}}'s stripping.)
##' @param ... further parameters for \code{\link[pls]{plsr}}, importantly, the number of PLS
##' variates to be used can be given via \code{ncomp}.
##' @return object of class "plslda", consisting of the mvr object returned by
##' \code{\link[pls]{plsr}} and the lda object returned by \code{\link[MASS]{lda}}.
##' @author Claudia Beleites
##' @seealso \code{\link[pls]{plsr}}, \code{\link[MASS]{lda}}
##' @export
##' @include cbmodels.R
##' @include center.R
plslda <- function (X, Y, grouping, comps = TRUE, ncomp = min (dim (X)), 
                    stripped = TRUE,
                    ...
                    #, subset = TRUE
                    #, na.action
                    ){

  tmp <- .ldapreproc (X = X, Y = Y, grouping = grouping)
    
  ## PLS
  pls <- plsr (formula = Y ~ X, data = data.frame (X = I(tmp$X), Y = I (tmp$Y)),
               ...,
               center.x = tmp$center.x, center.y = tmp$center.y, 
               method = "kernelpls", 
               stripped = FALSE,
               ncomp = ncomp
               )
  if (stripped) {
    pls$model <- NULL
    gc ()
    pls$fitted.values <- NULL
    pls$residuals <- NULL
    gc ()
  }
  
  scores <- as.matrix (predict (pls, type = "scores", comps = comps)) # as matrix needed in case of 1
                                                                      # lv only
	
  ## LDA
  lda <- lda (x = scores, grouping = tmp$grouping, ...)
	
  structure (list (pls = pls, lda = lda, comps = comps), class = "plslda")
}

##' 
##' @export 
##' @param object the plslda model
##' @param newdata matrix with new cases to be predicted.
##' @param ... further arguments for  \code{link[MASS]{predict.lda}} and
##' \code{link[pls]{predict.mvr}} 
##' @return list with results from \code{link[MASS]{predict.lda}} plus the pls scores used for LDA
##' prediction in element \code{$scores}
##' @seealso  \code{\link[MASS]{predict.lda}},  \code{\link[pls]{predict.mvr}}
##' \code{\link[cbmodels]{rotate}} for rotation of the LDA part of the model
##' @rdname plslda
##' @method predict plslda
##' @S3method predict plslda
predict.plslda <- function (object, newdata, ...){
  PLSscores <- predict (object$pls, newdata=newdata, type = "scores", comps = object$comps, ...)

  res <- predict (object$lda, newdata = PLSscores, ...)
  res$scores <- PLSscores

  ## avoid duplicated rownames warnings with as.data.frame
  rownames (res$scores) <- NULL
  rownames (res$x) <- NULL
  rownames (res$posterior) <- NULL
  
  res
}

##' @rdname plslda
##' @export
##' @method coef plslda
##' @S3method coef plslda
coef.plslda <- function (object, ...){
  object$pls$projection [, object$comps, drop = FALSE] %*% object$lda$scaling
}

##' @rdname plslda
##' @export
##' @method center plslda
##' @S3method center plslda
center.plslda <- function (object, ...){
  object$pls$Xmeans
}

.test (plslda) <- function (){
  X <- as.matrix (iris [,c ("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
  grp <- iris$Species

  complist <- list (TRUE, 1:3, 4)

  for (comps in complist) {
    model <- plslda (X = X, grouping = grp, comps = comps)

    ## x-centering of the model
    checkEqualsNumeric (colMeans (model$lda$means), rep (0, nrow (model$lda$scaling)),
                        msg = sprintf ("centering with comps = %s", comps))

    ## center (model)
    checkEqualsNumeric (model$pls$Xmeans, center (model),
                        msg = sprintf ("center (model); %s comps", comps))
    
   
    ## correct reconstruction of scores
    Xz <- scale (X, center = center (model), scale = FALSE)
    plsproj <- model$pls$projection [, model$comps, drop = FALSE]
    
    checkEqualsNumeric (Xz %*% plsproj,
                        predict (model$pls, type = "scores", comps = comps),
                        msg = sprintf ("scores with comps = %s", comps))


    ## correct total coefficients
    coef <- model$pls$projection [, comps, drop = FALSE] %*% coef (model$lda)
    checkEqualsNumeric (coef (model), coef,
                        msg = sprintf ("coefficients with comps = %s", comps))
    }
}
