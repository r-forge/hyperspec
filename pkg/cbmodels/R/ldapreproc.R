## common pretreatment of data for PLS-LDA and PCA-LDA

.ldapreproc <- function (X, Y, grouping, subset, na.rm = TRUE){
  ## produce both forms of Y data
  if (missing (Y))
    Y <- factor2matrix (grouping)
  if (! is.matrix (Y))
    Y <- as.matrix (Y)

  if (missing (grouping))
    grouping <- hardclasses (Y)

  ## deal with NAs
  nas <- is.na (grouping) | apply (is.na (X), 1, any)
  if (any (nas) & !na.rm) { 
    stop ("NAs not allowed if na.rm == FALSE.")
  } else {
    if (is.logical (subset))
      subset <- subset & !nas
    else if (is.numeric (subset))
      subset <- setdiff (subset, which (nas))
    else
      stop ("subset of type", class (subset), "not supported.")
  }

  ## centering parameters
  ## TODO: priors
  weights <- 1 / table (grouping) #  weights <- 1 / colSums (Y)
  weights <- as.numeric (weights [grouping]) # get rid of matrix dimensions
  
  Xmeans <- rowsum (X * weights, grouping)
  center.x <- colMeans (Xmeans)
  Ymeans <- rowsum (Y * weights, grouping)
  center.y <- colMeans (Ymeans)

  list (Y = Y,
        grouping = grouping,
        subset = subset,
        center.x = center.x,
        center.y = center.y)
}

.test (.ldapreproc) <- function (){

  ## subset

  ## center.x is tested indirectly in plslda and pcalda tests
  ## correct center.x leads to 

}
