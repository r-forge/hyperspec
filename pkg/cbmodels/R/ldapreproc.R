## common pretreatment of data for PLS-LDA and PCA-LDA
##' @include cbmodels.R
.ldapreproc <- function (X, Y, grouping#, subset = TRUE#, na.action
                         ){
  if (! is.matrix (X))
    X <- as.matrix (X)
  
  ## produce both forms of Y data
  if (missing (Y))
    Y <- factor2matrix (grouping)
  if (! is.matrix (Y))
    Y <- as.matrix (Y)

  if (missing (grouping))
    grouping <- hardclasses (Y)

  ## deal with NAs
  if (any (is.na (grouping)) || any (is.na (X)) || any (is.na (Y)))
    stop ("NA in data are not yet supported.")
  
  ## deal with NAs like lda does
  ## if (! missing (na.action)){
  ##   tmp <- na.action ()
  ##   X <- df$X
  ##   Y <- df$Y
  ##   grouping <- df$grouping
  ## }
    

  ## centering parameters
  ## TODO: priors
  weights <- 1 / table (grouping) #  weights <- 1 / colSums (Y)
  weights <- as.numeric (weights [grouping]) # get rid of matrix dimensions
  
  Xmeans <- rowsum (X * weights, grouping)
  center.x <- colMeans (Xmeans)
  Ymeans <- rowsum (Y * weights, grouping)
  center.y <- colMeans (Ymeans)

  list (X = X,
        Y = Y,
        grouping = grouping,
#        subset = subset,
        center.x = center.x,
        center.y = center.y)
}

.test (.ldapreproc) <- function (){

  X <- as.matrix (iris [,c ("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")])
  grp <- iris$Species

  tmp <- .ldapreproc (X = X, grouping = grp)
  lda <- lda (x = X, grouping = grp)
  pls <- kernelpls.fit (X = X, Y = tmp$Y, ncomp = 1)
  
  ## calculation of Y
  checkEquals (hardclasses (tmp$Y), grp)

  ## center.x
  checkEqualsNumeric (tmp$center.x, colMeans (lda$means))

  ## center.x is further tested indirectly in plslda and pcalda tests correct center.x leads to
  ## colMeans (lda$means)) == 0

  ## center.y
  checkEqualsNumeric (tmp$center.y, pls$Ymeans)  
  
  ## calculation of grouping
  tmp <- .ldapreproc (X = X, Y = factor2matrix (grp))
  checkEquals (tmp$grouping, grp)
 
  ## na.action
  grp.na <- sample (length (grp), 10)
  grp [grp.na] <- NA
  x.na <- sample (prod (dim (X)), 10)
  X [x.na] <- NA

  ## tmp <- .ldapreproc (X = X, grouping = grp)
  ## checkTrue (all (is.na (tmp$center.x)))
  ## checkTrue (all (is.na (tmp$center.y)))
  ## checkEquals (tmp$X, X)
  ## checkEquals (tmp$grouping, grp)


  ## tmp <- .ldapreproc (X = X, grouping = grp, na.action = na.pass)
  ## checkTrue (all (is.na (tmp$center.x)))
  ## checkTrue (all (is.na (tmp$center.y)))
  ## checkEquals (tmp$X, X)
  ## checkEquals (tmp$grouping, grp)
  

  ## tmp <- .ldapreproc (X = X, grouping = grp, na.action = na.exclude)


  ## tmp <- .ldapreproc (X = X, grouping = grp, na.action = na.omit)


  checkException (.ldapreproc (X = X, grouping = grp, na.action = na.fail))

}

