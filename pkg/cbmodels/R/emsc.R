
##' Extended Multiplicative Scatter Correction (EMSC)
##'
##' This function implements an EMSC similar to the approach described
##' in Martens2003 (see below). In contrast to the notation there,
##' \bold{all} reference spectra are given in one matrix.
##' Arguments \code{bg.comps} and \code{norm.comps} specify which spectra
##' belong to background components or should be considered for normalization.
##'
##' Polynomials can be generated with \code{\link[hyperSpec]{vanderMonde}}.
##' @param X spectra matrix in rows
##' @param Reference constituents matrix in rows
##' @param bg.comps background components indices
##' @param norm.comps normalization components indices
##' @param Reference matrix with reference spectra rows
##' @rdname emsc
##' @return new spectra matrix
##' @author C.Beleites & S. Fuller
##' @export
##' @seealso \code{\link[pls]{msc}} was used as starting point for this function.
##'
##' \code{\link[hyperSpec]{vanderMonde}}
##' @references Martens, H.; Nielsen, J. P. and Engelsen, S. B.: Light Scattering and Light
##' Absorbance Separated by Extended Multiplicative Signal Correction. Application to Near-Infrared
##' Transmission Analysis of Powder Mixtures, Analytical Chemistry (2003) 75, 394-404.
##' 
emsc <- function (X, Reference = NULL, bg.comps = NULL, norm.comps = NULL, ...) {
    
    if (is.null (Reference)){
      Reference <- rbind (1, colMeans (X))	#sanity check
    }
    
    if (is.vector (Reference)){	       # flip to 1-row matrix - bit of a hack -
                                        # redo solve etc. below with transposes for all cases? 
      Reference <- rbind (1, t (Reference))
    }
    
    B <- solve (tcrossprod (Reference), t (tcrossprod (X, Reference)))
    
    if (!is.null (bg.comps)){           
      X <- X - crossprod (B[bg.comps,,drop=FALSE],  
	                        Reference[bg.comps,,drop=FALSE])	
    }
    
    if (!is.null (norm.comps)){
      X <- sweep (X,
                  1,
                  rowMeans (Reference[norm.comps,,drop=FALSE]) 
			    	          %*% 
			            B[norm.comps,,drop=FALSE],
                  '/')
    }
    X
}

##' @noRd
setGeneric ("emsc")

##' @export
##' @return hyperSpec method: hyperSpec object containing emsc corrected spectrum of input hyperSpec object X, given matrix: Reference, vectors: bg.comps, norm.comps, indices.
##' @rdname emsc
setMethod ("emsc", signature = signature (X = "hyperSpec", Reference = "numeric"),
           function (X, Reference, bg.comps, norm.comps, ...){
  
  validObject (X)
  chk.hy (X)

  X@data$spc <- emsc (X@data$spc, Reference, bg.comps, norm.comps)
  
  X
})

##' @export
##' @rdname emsc
setMethod ("emsc", signature = signature (X = "hyperSpec", Reference = "matrix"),
           function (X, Reference, bg.comps, norm.comps, ...){
  
  validObject (X)
  chk.hy (X)
  X@data$spc <- emsc (X@data$spc, Reference, bg.comps, norm.comps)
  
  X
})

##' @export
##' @rdname emsc
setMethod ("emsc", signature = signature (X = "hyperSpec", Reference = "missing"),
           function (X, Reference, bg.comps, norm.comps, ...){
  
  validObject (X)
  chk.hy (X)
  X@data$spc <- emsc (X@data$spc,, bg.comps, norm.comps)
  
  X
})

##' @param \dots hyperSpec method: further arguments to \code{\link{decomposition}}
##' @export
##' @return hyperSpec method: hyperSpec object containing emsc corrected spectrum of input hyperSpec objects: X and Reference, vectors: bg.comps and norm.comps, indices.
##' @rdname emsc 
##' @examples
##' vmflu <- vanderMonde (flu, 2, normalize.wl = normalize01)
##' Refs <- collapse (normalize01 (flu[6,]), vmflu)
##' plot (Refs)
##' plot (emsc (flu, Refs, bg.comps = 2 : 4, norm.comps = 1))
setMethod ("emsc", signature = signature (X = "hyperSpec", Reference = "hyperSpec"), function (X, Reference, bg.comps, norm.comps, ...){
  
  validObject (X)
  chk.hy (X)
  validObject (Reference)
  chk.hy (Reference)
  
  X@data$spc <- emsc (X@data$spc, Reference@data$spc, bg.comps, norm.comps)
  
  X
})

##' @include cbmodels.R
.test (emsc) <- function (){
  m <- vanderMonde (1:5, 2)
  v <- 1 : 3

  tmp <- emsc (m, v)
  checkEquals (tmp, m)
  
  ## calculation checks
  x <- vanderMonde (1:5, 2) %*% 1 : 3

  ## if no bg.comps and no norm.comps are given, the original should be returned
  checkEqualsNumeric (emsc (t (x), t (vanderMonde (1:5, 2))), t (x))

  checkEqualsNumeric (emsc (t (x), t (vanderMonde (1:5, 2)), bg.comps = 1),
                      t (x - vanderMonde (1:5, 2) %*% c (1, 0, 0)))
  checkEqualsNumeric (emsc (t (x), t (vanderMonde (1:5, 2)), bg.comps = 2),
                      t (x - vanderMonde (1:5, 2) %*% c (0, 2, 0)))
  checkEqualsNumeric (emsc (t (x), t (vanderMonde (1:5, 2)), bg.comps = 3),
                      t (x - vanderMonde (1:5, 2) %*% c (0, 0, 3)))
  
  checkEqualsNumeric (emsc (t (x), t (vanderMonde (1:5, 2)), bg.comps = 1:2),
                      t (vanderMonde (1:5, 2) %*% c (0, 0, 3)))
  
  checkEqualsNumeric (emsc (t (x), t (vanderMonde (1:5, 2)), bg.comps = 1:3),
                      matrix (rep (0, length (x))))

  if (require ("hyperSpec", quietly = TRUE)){

    ##the hyperSpec object should be otherwise unchanged
    checkEquals (emsc (flu), flu)
    checkEquals (emsc (flu, wl (flu)), flu)
    checkEquals (emsc (flu, flu [[]]), flu)
    checkEquals (emsc (flu, flu), flu)

    checkEqualsNumeric (emsc (flu, vanderMonde (flu, 2, normalize.wl=normalize01))[[]], 
                        emsc (flu, t (vanderMonde (normalize01 (wl (flu)), 2)))[[]])
  }
}
