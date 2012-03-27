
plslda <- function (X, Y, grouping, ...){
  require (pls)
  require (MASS)
  
	## PLS
	pls <- plsr (formula = Y ~ X, data = data.frame (X = I(X), Y = Y), ...)	
	scores <- predict (pls, type = "scores")
	
	## LDA
	lda <- lda (x = scores, grouping = grouping, ...)
	
	structure (list (pls = pls, lda = lda), class = "plslda")
}

predict.plslda <- function (model, newdata, ...){
	PLSscores <- predict (model$pls, newdata=newdata, type = "scores", ...)
	res <- predict (model$lda, newdata = PLSscores, ...)
	res$scores <- PLSscores
	
	res
}

