#  File src/library/base/R/scale.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  2012-05-22 C. Beleites: replace sweep by faster subtraction of 
#  `rep`eated center and scale, respectively.

scale <- function(x, center = TRUE, scale = TRUE) UseMethod("scale")

message ("Installing faster replacement for base::scale.")

scale <- function(x, center = TRUE, scale = TRUE){
    x <- as.matrix(x)
    nc <- ncol(x)
    if (isTRUE (center)) {
            center <- colMeans(x, na.rm=TRUE)
    }

    if (is.numeric(center)) {
      if (length (center) != ncol (x))
        stop("length of 'center' must equal the number of columns of 'x'")
      
      x <- x - rep (center, each = nrow (x))
    }
          
    if (isTRUE (scale)) {
      scale <- colSums (x^2, na.rm = TRUE)
        
      n <- nrow (x) - colSums (is.na (x)) - 1L
      n [n == 0L] <- 1

      scale <- sqrt (scale / n)
    }

    if (is.numeric(scale)){
      if (length (scale) != ncol (x))
        stop("length of 'center' must equal the number of columns of 'x'")

      x <- x * rep (1 / scale, each = nrow (x))

    }
       
    if(is.numeric(center)) attr(x, "scaled:center") <- center
    if(is.numeric(scale)) attr(x, "scaled:scale") <- scale
    x
}

.test (scale) <- function (){
  m <- matrix (1:12, 3, 4)
  checkEquals (scale (m), base::scale (m))

  checkEquals (scale (m, center = FALSE), base::scale (m, center = FALSE))
  checkEquals (scale (m, scale = FALSE), base::scale (m, scale = FALSE))

  checkEquals (scale (m, center = FALSE), base::scale (m, center = FALSE))
  checkEquals (scale (m, scale = FALSE), base::scale (m, scale = FALSE))

  checkEquals (scale (m, center = FALSE, scale = FALSE),
               base::scale (m, center = FALSE, scale = FALSE))

  checkEquals (scale (m, center = 1 : 4), base::scale (m, center = 1 : 4))
  checkEquals (scale (m, scale = 1 : 4), base::scale (m, scale = 1 : 4))

  checkEquals (scale (m, center = 1 : 4, scale = 1 : 4),
               base::scale (m, center = 1 : 4, scale = 1 : 4))

}
