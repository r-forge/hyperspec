\name{lscb}
\alias{lscb}
\title{Detailed listing of objects}
\usage{
  lscb(names = ls(envir = parent.frame()), sort = "size",
    total = TRUE)
}
\arguments{
  \item{names}{names of the object to list}

  \item{sort}{column to sort}

  \item{total}{logical indicating whether the total size
  should be printed as well}
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

