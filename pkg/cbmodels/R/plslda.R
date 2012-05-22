##' LDA models using PLS latent variables as input space.
##'
##' @title PLS-LDA
##' @param X input variate matrix
##' @param Y matrix with class membership (classes correspond to columns).  If missing,
##' \code{\link[softclassval]{factor2matrix} (grouping)} is used.
##' @param grouping factor with class membership. If missing, \code{\link[softclassval]{hardclasses}
##' (Y)} is used.
##' @param ... further parameters for \code{\link[pls]{plsr}}, importantly, the number of PLS
##' variates to be used can be given via \code{ncomp}.
##' @return object of class "plslda", consisting of the mvr object returned by
##' \code{\link[pls]{plsr}} and the lda object returned by \code{\link[MASS]{lda}}.
##' @author Claudia Beleites
##' @seealso \code{\link[pls]{plsr}}, \code{\link[MASS]{lda}}
##' @export 
plslda <- function (X, Y, grouping, ...){
  if (missing (Y))
    Y <- factor2matrix (grouping)

  if (missing (grouping))
    grouping <- hardclasses (Y)
    
	## PLS
	pls <- plsr (formula = Y ~ X, data = data.frame (X = I(X), Y = Y), ...)	
	scores <- predict (pls, type = "scores")
	
	## LDA
	lda <- lda (x = scores, grouping = grouping, ...)
	
	structure (list (pls = pls, lda = lda), class = "plslda")
}

##' 
##' @export 
##' @param object the plslda model
##' @param newdata matrix with new cases to be predicted.
##' @param ... further arguments for  \code{link[MASS]{predict.lda}} and
##' \code{link[pls]{predict.mvr}} 
##' @return list with results from \code{link[MASS]{predict.lda}} plus the pls scores used for LDA
##' prediction in element \code{$scores}
##' @seealso  \code{link[MASS]{predict.lda}},  \code{link[pls]{predict.mvr}}
##' @rdname plslda
predict.plslda <- function (object, newdata, ...){
  PLSscores <- predict (object$pls, newdata=newdata, type = "scores", ...)
  res <- predict (object$lda, newdata = PLSscores, ...)
  res$scores <- PLSscores
	
  res
}

