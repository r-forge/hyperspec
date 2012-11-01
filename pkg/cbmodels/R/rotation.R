
##' Calculate a rotation matrix
##'
##' The rotation will take place in the plane spanned by dimensions \code{dim1}, \code{dim2}. All
##' other directions are left untouched.
##'
##' Matrix-multiplying \code{ndim}-dimensional points with the rotation matrix rotates the points
##' \code{alpha} radians clockwise, see the example.
##' @title rotmat
##' @param alpha rotation angle (in rad)
##' @param ndim number of dimensions
##' @param dim1,dim2 the plane in which the rotation should take place
##' @return matrix of size (\code{ndim} x \code{ndim}), 
##' @author Claudia Beleites
##' @export
##' @examples
##'
##' rotmat (alpha = pi/2)
##' rotmat (alpha = 1)
##'
##' point <- t (c (1, 1))
##' R <- rotmat (pi/20)
##' R
##' plot (point, ylim = c (0, 1.2), xlim = c (0, 1.2), pch = 19)
##' ## clockwise rotation
##' points (point %*% R)
##' ## counterclockwise rotation
##' points (point %*% t (R), col = 2)

rotmat <- function (alpha = 0, ndim = 2, dim1 = 1, dim2 = 2){
  R <- diag (ndim)
  R [c (dim1, dim2), c (dim1, dim2)] <- c (cos (alpha), sin (alpha), -sin (alpha), cos (alpha))

  R
}

.test (rotmat) <- function (){
  checkEqualsNumeric (rotmat (alpha = pi/2), c (0, 1, -1, 0))

  ndim <- 5
  dim1 <- 2
  dim2 <- 4
  R <- rotmat (alpha = pi/2, ndim = ndim, dim1 = dim1, dim2 = dim2)
  checkEqualsNumeric (R %*% t (R), diag (ndim))
  checkEqualsNumeric (R [c (1, 3, 5), c (1, 3, 5)], diag (5 - 2))
  checkEqualsNumeric (R [c (dim1, dim2), c (dim1, dim2)], c (0, 1, -1, 0))
  checkEqualsNumeric ((1:5) %*% R, c (1, 4, 3, -2, 5))

  R <- rotmat (alpha = 1, ndim = ndim, dim1 = dim1, dim2 = dim2) # weird angle: 180/pi = ca. 57.3°
  checkEqualsNumeric (R %*% t (R), diag (ndim))
  checkEqualsNumeric (R [c (1, 3, 5), c (1, 3, 5)], diag (5 - 2))
  checkEqualsNumeric (R [c (dim1, dim2), c (dim1, dim2)], rotmat (alpha = 1))
}

