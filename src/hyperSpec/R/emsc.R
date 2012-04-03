
##' Function for doing EMSC on hyperSpec objects
##'
##' @param X spectra matrix in rows
##' @param Reference matrix with reference spectra rows
##' @param bg.comps indices of background components
##' @param norm.comps indices of components to use for normalization
##' @rdname emsc
##' @return new spectra matrix
##' @author S. Fuller
##' @export
##' @seealso \code{\link[pls]{msc}} was used as starting point for this function.
##' @references Martens, H.; Nielsen, J. P. and Engelsen, S. B.: Light Scattering and Light
##' Absorbance Separated by Extended Multiplicative Signal Correction. Application to Near-Infrared
##' Transmission Analysis of Powder Mixtures, Analytical Chemistry (2003) 75, 394-404.
##' 
emsc <- function( X, Reference = NULL, bg.comps = NULL, norm.comps = NULL, ... ) {
    if( is.null( Reference ) ){
      Reference <- rbind(1, colMeans( X ) )	#sanity check
    }
    if( is.vector( Reference ) ){	#flip to 1-row matrix - bit of a hack - redo solve etc. below with transposes for all cases? 
      Reference <- rbind(1, t(Reference))
    }
    B <- solve( tcrossprod(Reference), t(tcrossprod(X, Reference)) )
    if( !is.null( bg.comps ) ){           
      X <- X - crossprod( matrix(B[bg.comps,], length(bg.comps), ncol(B), byrow=TRUE ),  
	                           matrix(Reference[bg.comps,], length(bg.comps), ncol(Reference), byrow=TRUE ) )	
    }
    if( !is.null( norm.comps ) ){
      X <- sweep( X,
                  1,
                  rowMeans( matrix(Reference[norm.comps,], length(norm.comps), ncol(Reference), byrow=TRUE )  ) 
			    	%*% 
			    matrix( B[norm.comps,], length(norm.comps), ncol(B), byrow=TRUE ),
                  '/' )
    }
    X
}
	
##' @noRd
#setGeneric ("emsc")

##' @param \dots hyperSpec method: further arguments to \code{\link{decomposition}}
##' @export
##' @return hyperSpec method: hyperSpec object containing emsc corrected spectrum of input hyperSpec object X, given matrix: Reference, vectors: bg.comps, norm.comps, indices.
##' @rdname emsc
setMethod( "emsc", signature = signature ( X = "hyperSpec" ), function ( X, Reference, bg.comps, norm.comps, ... ){
  validObject( X )
  X <- decomposition( X, emsc( X$spc, Reference, bg.comps, norm.comps ), scores = FALSE, ...)
  X
} )

##' @param \dots hyperSpec method: further arguments to \code{\link{decomposition}}
##' @export
##' @return hyperSpec method: hyperSpec object containing emsc corrected spectrum of input hyperSpec objects: X and Reference, vectors: bg.comps and norm.comps, indices.
##' @rdname emsc 
setMethod( "emsc", signature = signature ( X = "hyperSpec", Reference = "hyperSpec" ), function ( X, Reference, bg.comps, norm.comps, ... ){
  validObject( X )
  X <- decomposition( X, emsc( X$spc, Reference$spc, bg.comps, norm.comps), scores = FALSE, ... )
  X
} )


##' @param \dots hyperSpec method: further arguments to f
##' @return hyperSpec method: hyperSpec object containing emsc corrected spectrum of input hyperSpec objects: X and Reference, vectors: bg.comps and norm.comps, indices.
##' @rdname emsc
##' @examples
##' vmflu <- vanderMonde(flu,2)
##' Refs <- rbind(1, jitter(flu[[]][1,], 100), vanderMonde(flu,2,normalize01)[[]][c(2,3),])
##' nuflu <- emsc(flu, Refs, 1, c(2,3))

spc.evalfun <- function(X, f, ... ){
	chk.hy(X)
	validObject(X)
	tailargs <- list(...)
	pars <- append( list(f$spc), tailargs)
	X <- decomposition (X, do.call(f, tailargs), scores = FALSE, ...)
	X
}
