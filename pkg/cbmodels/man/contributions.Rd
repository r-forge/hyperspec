\name{contributions}
\alias{contributions}
\alias{contributions.lda}
\alias{contributions.pcalda}
\title{contriubutions}
\usage{
  contributions(object, newdata, dims = TRUE, ...)

  \method{contributions}{lda} (object, newdata,
    dims = TRUE, prior = object$prior, ...)

  \method{contributions}{pcalda} (object,
    newdata = stop("newdata is required: ", "pcalda models do not store the original data"),
    dims = TRUE, ...)
}
\arguments{
  \item{object}{a model}

  \item{newdata}{data to use for calculating contributions}

  \item{dims}{dimensions in model space of which the
  contributions should be calculated}

  \item{...}{further parameters}

  \item{prior}{For \code{\link[MASS]{lda}} models, priors
  can be given.}
}
\value{
  an array of dimension (\code{nrow (newdata)} x \code{ncol
  (newdata)} x \code{length (dims)})
}
\description{
  Contributions of Variates to Model Predictions
}
\details{
  Linear models procduce scores according to \deqn{S = (X -
  center) \times coefficients)}{S = (X - center) x
  coefficients} from the data matrix X, the center of the
  model and the model coefficients (scaling, rotation,
  loadings, latent variables, ...)

  To study the behaviour of the model it is often useful to
  calculate the element-wise product of centered data and
  the for each latent variable (component, discriminant
  function, ...). This is done by \code{contributions}
}
\examples{
## contributions 1st discriminant function
model <- lda (Species ~ ., data = iris)
contrib.LD1 <- contributions (model, dims = 1)
contrib.LD1[1:6,,]
dim (contrib.LD1)
layout (1:3, 3, 1)
for (class in levels (iris$Species))
   boxplot (contrib.LD1 [iris$Species == class,,], ylim = range (contrib.LD1))
## all contributions
contrib.LD <- contributions (model)
dim (contrib.LD)
contrib.LD[1:6,,]

if (require ("reshape2")) {
  contrib.df <- melt (contrib.LD, value.name = "contribution")
} else {
  contrib.df <- array2df (contrib.LD, label.x = "contribution")
}

contrib.df$Species <- iris$Species [contrib.df$row]
head (contrib.df)

if (require ("lattice")){
  bwplot (contribution ~ Species | variate, data = contrib.df, layout = c (4, 1))
}

## sum contributions to get scores
diff <- predict (model)$x - apply (contrib.LD, c (1, 3), sum)
summary (diff)
boxplot (diff)
1+1
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[MASS]{lda}}

  \code{\link[cbmodels]{pcalda}}
}

