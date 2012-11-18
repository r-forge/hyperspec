##' @seealso 
##' \code{\link[MASS]{lda}} 
##' @export
##' @import contributions.R
##' @method contributions lda
##' @S3method contributions lda
##' @rdname contributions
##' @examples
##'
##' ## contributions 1st discriminant function
##' model <- lda (Species ~ ., data = iris)
##' contrib.LD1 <- contributions (model, dimen = 1)
##' contrib.LD1[1:6,,]
##' dim (contrib.LD1)
##' layout (1:3, 3, 1)
##' for (class in levels (iris$Species))
##'    boxplot (contrib.LD1 [iris$Species == class,,], ylim = range (contrib.LD1))
##' ## all contributions
##' contrib.LD <- contributions (model)
##' dim (contrib.LD)
##' contrib.LD[1:6,,]
##' 
##' if (require ("reshape2")) 
##'   contrib.df <- melt (contrib.LD, value.name = "contribution")
##' else
##'   contrib.df <- array2df (contrib.LD, label.x = "contribution")
##' 
##' contrib.df$Species <- iris$Species [contrib.df$row]
##' head (contrib.df)
##'   
##' if (require ("lattice")){
##'   bwplot (contribution ~ Species | variate, data = contrib.df, layout = c (4, 1))
##' }
##' 
##' ## sum contributions to get scores
##' diff <- predict (model)$x - apply (contrib.LD, c (1, 3), sum)
##' summary (diff)
##' boxplot (diff)


contributions.lda <- function (object, newdata, prior = object$prior, dimen = TRUE) {
## largely copied from MASS:::predict.lda

    if(!inherits(object, "lda")) stop("object not of class \"lda\"")
    if(!is.null(Terms <- object$terms)) { #
    # formula fit
        Terms <- delete.response(Terms)
        if(missing(newdata)) newdata <- model.frame(object)
        else {
            newdata <- model.frame(Terms, newdata, na.action=na.pass,
                                   xlev = object$xlevels)
            if (!is.null(cl <- attr(Terms, "dataClasses")))
                .checkMFClasses(cl, newdata)
        }
        x <- model.matrix(Terms, newdata, contrasts = object$contrasts)
        xint <- match("(Intercept)", colnames(x), nomatch=0L)
        if(xint > 0) x <- x[, -xint, drop=FALSE]
    } else { #
    # matrix or data-frame fit
        if(missing(newdata)) {
            if(!is.null(sub <- object$call$subset))
                newdata <-
                    eval.parent(parse(text=paste(deparse(object$call$x,
                                      backtick=TRUE),
                                      "[", deparse(sub, backtick=TRUE),",]")))
            else newdata <- eval.parent(object$call$x)
            if(!is.null(nas <- object$call$na.action))
                newdata <- eval(call(nas, newdata))
        }
        if(is.null(dim(newdata)))
            dim(newdata) <- c(1, length(newdata))  # a row vector
        x <- as.matrix(newdata)		# to cope with dataframes
    }

    if(ncol(x) != ncol(object$means)) stop("wrong number of variables")
    if(length(colnames(x)) > 0L &&
      any(colnames(x) != dimnames(object$means)[[2L]]))
         warning("variable names in 'newdata' do not match those in 'object'")
    ng <- length(object$prior)
    if(!missing(prior)) {
        if(any(prior < 0) || round(sum(prior), 5) != 1) stop("invalid 'prior'")
        if(length(prior) != ng) stop("'prior' is of incorrect length")
    }
#   remove overall means to keep distances small
    means <- colSums(prior*object$means)
## end copy from MASS:::predict.lda

    .contributions (x = x, center = means, coef = object$scaling, dimen = dimen)
}

.test <- function (){
  require ("MASS")
  object <- lda (Species ~ ., iris)
  pred.lda <- predict (object)$x

  contributions <- contributions (object)

  checkEqualsNumeric (apply (contributions, c (1, 3), sum), pred.lda)
  checkEqualsNumeric (contributions [,,2], contributions (object, dimen = 2))
  checkEqualsNumeric (contributions [,,2], contributions (object, dimen = -1))
  checkEqualsNumeric (contributions, contributions (object, iris))
  checkEqualsNumeric (contributions [1:10,,2], contributions (object, iris [1:10,], dimen = -1))
}
