\name{rotmat}
\alias{rotmat}
\title{rotmat}
\usage{
  rotmat(alpha = 0, ndim = 2, dim1 = 1, dim2 = 2)
}
\arguments{
  \item{alpha}{rotation angle (in rad)}

  \item{ndim}{number of dimensions}

  \item{dim1,dim2}{the plane in which the rotation should
  take place}
}
\value{
  matrix of size (\code{ndim} x \code{ndim}),
}
\description{
  Calculate a rotation matrix
}
\details{
  The rotation will take place in the plane spanned by
  dimensions \code{dim1}, \code{dim2}. All other directions
  are left untouched.

  Matrix-multiplying \code{ndim}-dimensional points with
  the rotation matrix rotates the points \code{alpha}
  radians clockwise, see the example.
}
\examples{
rotmat (alpha = pi/2)
rotmat (alpha = 1)

point <- t (c (1, 1))
R <- rotmat (pi/20)
R
plot (point, ylim = c (0, 1.2), xlim = c (0, 1.2), pch = 19)
## clockwise rotation
points (point \%*\% R)
## counterclockwise rotation
points (point \%*\% t (R), col = 2)
}
\author{
  Claudia Beleites
}

