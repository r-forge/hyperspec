###-----------------------------------------------------------------------------
###
### blend  
###  
###


.blend.smooth <- function (x, y1, y2){
	a <- seq (from = 0, to = 1, length.out = ncol (y1))
	a <- matrix (rep (a, each = nrow (y1)), nrow (y1))
	y1 * (1 - a) + y2 * a 
}

.blend.mean  <- function (x, y1, y2){
	(y1 + y2) / 2
}

.blend.first <- function (x, y1, y2){
	y1
}

.blend.last <- function (x, y1, y2){
	y2
}

.blend.less <- function (x, y1, y2){
	a <- y1 < y2
	y2 [a] <- y1 [a]
	y2
}

blend <- function(..., fun = .blend.smooth, short = "blend", date = NULL, user = NULL) {
	dots <- list (...)

	if ((length (dots) == 1) & is.list (dots [[1]]))
		dots <- dots[[1]]
	
	if (length (dots) == 0)
		return (NULL)
	else if (length (dots) == 1){
		validObject (dots[[1]])
		return (dots[[1]])
	} else if (! all (sapply (dots, is, "hyperSpec")))
		stop ("blend only works on hyperSpec objects.")
	lapply (dots, validObject)
	
	if (length (unique (sapply (dots, nrow))) != 1)
		stop ("All objects need to have the same number of rows,.")
	
	logs <- list() 

	## sort by columns of @data: ensures binding matrices is sufficient.
	ord <- do.call (order, dots[[1]]$..)
	dots[[1]]@data <- dots[[1]]@data[ord, , drop = FALSE]
	
	wl <- sort (unique (unlist (lapply (dots, slot, "wavelength"))))
	
	spc <- matrix (NA, nrow (dots [[1]]), length (wl))
	done <- rep (FALSE, length (wl))
	for (i in seq_len (length (dots)) [-1]){
		ord <- do.call (order, dots[[i]]$..)
		dots[[i]]@data <- dots[[i]]@data[ord, , drop = FALSE]
		
		
		dots[[1]] <- fun (dots[[1]], dots[[i]])
			
			
		}
		dots[[1]]
	}	
} 


library (hyperSpec)
p <- spc.fit.poly.below (cbind (chondro [,, 600:700], chondro [,, 1500:1530]), chondro [,,600:1530])

debug (spc.eval.poly)

bl <- spc.eval.poly (chondro, p, 600:1530)




