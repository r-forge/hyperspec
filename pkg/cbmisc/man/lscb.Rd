\name{lscb}
\alias{lscb}
\title{Detailed listing of objects}
\usage{
  lscb(names = ls(envir = parent.frame()), sort = "size")
}
\arguments{
  \item{names}{names of the object to list}

  \item{sort}{column to sort}
}
\value{
  data.frame with columns giving name, class, length, dim
  and size of the objects.
}
\description{
  Detailed listing of objects
}
\author{
  Claudia Beleites
}

