require ("inline")
.center <- cxxfunction(signature(x = "numeric", center = "numeric"), body = '
   Rcpp::NumericMatrix _x (x);
   Rcpp::NumericVector _center (center);

   int nrows = _x.nrow ();
   int ncols = _x.ncol ();
   int ofs;
   double c;

   for (int j = 0; j < ncols; j++){
      c = - _center [j];
      ofs = nrows * j;
      for (int i = 0; i < nrows; i++){
         _x [ofs + i] += c;
      }
   }
   return _x;
', plugin="Rcpp")

.scale <- cxxfunction(signature(x = "numeric", scale = "numeric"), body = '
   Rcpp::NumericMatrix _x (x);
   Rcpp::NumericVector _scale (scale);

   int nrows = _x.nrow ();
   int ncols = _x.ncol ();
   int ofs;
   double s;

   for (int j = 0; j < ncols; j++){
      s = 1 / _scale [j];
      ofs = nrows * j;
      for (int i = 0; i < nrows; i++){
         _x [ofs + i] *= s;
      }
   }
   return _x;
', plugin="Rcpp")

library (microbenchmark)
library (hyperSpec)

m <- chondro [[]]
cnt <- as.numeric (chondro [[3,]])
sc <- rep (2, ncol (m))

timing <- microbenchmark (
  base:::scale (m, center = cnt, scale=sc),
  scale (m, center = cnt, scale=sc),
  .center (m, center = cnt),
  .scale (m, scale=sc),
  times = 10
)
