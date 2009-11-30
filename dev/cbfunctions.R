# Spur
tr <- function (x) { sum (diag (x)) }

# Sensitivität
sens <- function (x, class) {
  x[class, class] / sum (x[class,])
}

#Spezifizität
spez <- function (x, class) {
    sum (x[-class, -class]) / sum (x[-class,])
}

# majority vote
mv <- function (x, class, cutoff = 0) {
  if (length (which(!is.na(x))) == 0)
    NaN
  else {
    x <- summary (factor (x[!is.na(x)], levels=class));
	 #cat (x, "\n")
	 dummy <- which.max (x);
	 if (x[dummy] / sum (!is.na(x)) > cutoff)
    	names (dummy)
	 else
		NaN		
  }
}
# confusion matrix

confmat <- function (pred, truth, levels = levels(as.factor(truth))) {
  dummy <- split (pred[!is.na(pred)], factor(truth[!is.na(pred)], levels=levels))
  #cat (names (dummy))
#  print (levels(as.factor(truth)))
  if (length (dummy) == 0) # no predictions done
    matrix (nrow = length (levels), ncol = length (levels))
  else {
     colname <- levels(as.factor(unlist(dummy)));
	  dummy <- lapply (dummy, factor, levels=levels)
	  dummy <- lapply (dummy, summary);
	 #print (dummy)
    #cat ("\n")
 #   print (dummy)
 #   cat ("nrow: ", length(dummy), "el: ", length (unlist (dummy)))
     dummy <- matrix (unlist (dummy), nrow=length(dummy), byrow=TRUE);
	  #print (dummy)
     rownames (dummy) = levels;
	  colnames (dummy) = colname;
	  dummy
  }
}

TIKZboxwhisker <- function (x, hint=NULL, xpos="X", farbe="FARBE", dy="DYmm"){
  prc <- quantile (x, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm=TRUE)
  out <- x[x > prc[5] | x < prc[1]];
  mindist <- (x[length(x)] - x [1]) * 0.025; # kleinster unterscheidbarer Abstand: 2.5 % von der Gesamtspanne 
  # out <- sort (unique (out[!is.na(out)]))
  out <- sort (unique (round (out[!is.na(out)] / mindist) * mindist))
  
	prc <- format (prc, digits = 4);
	out <- format (out, digits=4);
  paste ("\\boxwhisker{", xpos, 
		  "}{", prc[1], 
		  "}{", prc[2], 
		  "}{", prc[3], 
		  "}{", prc[4], 
		  "}{", prc[5], 
		  "}{", paste (out, collapse=", "), 
		  "}{", farbe, 
		  "}{", dy, 
		  "}     % ", hint, "\n", sep="");
}

TIKZline <- function (x, y, hint=NULL, cycle=FALSE, options=NULL){
  x <- format (x, digits=4, scientific=FALSE);
  y <- format (y, digits=4, scientific=FALSE);
  cat ("\\draw [", options, "] (", sep="")
  cat (matrix (c(x, y), nrow=2, byrow=TRUE), sep=c(", ", ") -- ("))
  if (cycle) 
	cat (") -- cycle; % ", hint, "\n", sep="")
  else
     cat ("); % ", hint, "\n", sep="")
}

TIKZSpektren <- function (x, y, hint=NULL, options=NULL){

}

get.mesh <- function (x, y) {
	grid.x <- sort (unique (x));
	grid.y <- sort(unique (y));
	
	mesh <- matrix (nrow = length (grid.x), ncol = length (grid.y));
	
	for (i in 1:length(x)) {
		z <- which (grid.x == x[i]);
		s <- which (grid.y == y[i]);
		mesh[z,s] <- i;
	}
	
	mesh
}



########################################################################
#
plot.map <- function (x, y, value, ...) {
	mesh <- get.mesh (x, y);
	m <- matrix (value [mesh], ncol = ncol(mesh))
	#levelplot (m, xlab = sort (unique (x)), ylab = sort (unique (y)), ...)
	image (m, ...)		
}
########################################################################
#
modell.tabelle <- function (x, by) {
	dummy <- split (x$Probe, by)
	dummy2 <-  split (x$Patient, by, drop = TRUE)
	cat ("\\textbf{Class}   &  \\textbf{# Patients}  &  \\textbf{# Samples}  &  \\textbf{# Spectra} \\\\\\hline")
	for (i in 1 : length (dummy)) {
		cat ("\n", names(dummy)[i], "  & ", length (unique (dummy2[[i]])),
				"  & ", length (unique (dummy [[i]])),
				"  & ", length (dummy[[i]]), " \\\\")
	}
	cat ("\\hline\ntotal  & ", length (unique (x$Patient [by > 0])),
			    "  & ", length (unique (x$Probe   [by > 0])),
			    "  & ", sum (!is.na (by)), " \n")
}

########################################################################
#
# Resampling-Funktion: Bootstrap
#

resample.boot <- function (class, patient, nges, fboot.pat) {
	nclass <- round (nges / length (unique (class[!is.na (class)])))
	index <- t (rbind (1: length (class), patient))
	colnames (index) <- c("index", "patient")
	index <- split (index, class)
	index <- lapply (index, matrix, ncol=2)
	index <- lapply (index, function (x){split (x[,1], x[,2])})
	
#	len <- sapply (index, length)
#	index <- index [order (len)] 	# steigende Pat.zahlen in den Klassen:
#											# Bereits zum Training gezogene Pat. 
#											# mischen die Belegung der weiteren 
#											# Klassen möglichst wenig auf
#	
	pat.train <- list ();
	spc.train <- list ();
	i <-  1
	for (i in 1 : length (index)) {
#		npat <- round (nclass / length (index [[i]]));
		pats <- as.integer (names (index[[i]]));
		pat.train [[i]] <- c (pats [pats %in% unlist (pat.train)], 
				sample (pats, 
						length (index [[i]]) * fboot.pat - sum (pats %in% unlist (pat.train)),
						replace = TRUE
						)
				); # bootstrap resampling
		
		spc.train [[i]] <- lapply (index[[i]][names (index[[i]]) %in% pat.train[[i]]],
				sample, round (nclass / length (unique (pat.train [[i]]))),
				replace = TRUE
				);
	}
	
	pat.test <- unique (patient) [!unique (patient) %in% (unique (unlist (pat.train)))]
	spc.test <- (1: length (class)) [patient %in% pat.test]
	
	list (train.pat = pat.train, train.spc = spc.train, 
			test.pat = pat.test, test.spc = spc.test)
	
}
########################################################################
#
## array2vec <- function (i, dim){
##   if (!is.matrix (i))
##     dim (i) <- c(1, 1)

##   cp <- c(1, cumprod (dim))[1:length (dim)]
##   i <- i - 1
  
##   colSums(apply (i, 1, "*", cp)) + 1
## }

########################################################################
##
## vec2array <- function (l, dim) {
##     ndim <- length(dim)
##     pi <- cumprod(c(1, dim))
##     j <- matrix(NA, nrow = length(l), ncol = ndim)
##     colnames (j) <- letters[9 : (9 + ndim - 1)]
##     l <- (l - 1)
##     for (kk in 1:ndim) j[, kk] <- (l%%pi[kk + 1])/pi[kk]
##     1 + floor(j)
## }


########################################################################
#
## count.rows <- function(x) { 
## 	order.x <- do.call(order,as.data.frame(x))
## 	if (is.vector (x)) {
## 		equal.to.previous <-
## 				x[tail(order.x,-1)] == x[head(order.x,-1)]
## 	} else {
## 		equal.to.previous <-
## 			rowSums(x[tail(order.x,-1),] != x[head(order.x,-1),])==0
## 	}	
## 	tf.runs <- rle(equal.to.previous)
## 	counts <- c(1,
## 			unlist(mapply( function(x,y) if (y) x+1 else (rep(1,x)),
## 							tf.runs$length, tf.runs$value )))
## 	counts <- counts[ c(diff(counts) <= 0, TRUE ) ]
## 	unique.rows <- which( c(TRUE, !equal.to.previous ) )
## 	if (is.vector (x)) {
## 		cbind( counts, x[order.x[ unique.rows ], drop=F] )
## 	} else {
## 		cbind( counts, x[order.x[ unique.rows ], , drop=F] )
## 	}
## }
########################################################################
#
count.rows <- function(x) { 
  if (is.matrix (x) && (dim (x) [2] == 1))
    x <- as.vector (x) 

  order.x <- do.call(order,as.data.frame(x))
  if (is.vector (x)) {
    equal.to.previous <-
      x[tail(order.x,-1)] == x[head(order.x,-1)]
  } else {
    equal.to.previous <-
      rowSums(x[tail(order.x,-1),] != x[head(order.x,-1),])==0
  }

  indices <- split (order.x, cumsum (c (TRUE, !equal.to.previous)))

  if (is.vector (x)) {
    x <- x [sapply (indices, function (v) v [[1]]),   drop = FALSE]
  } else {
    x <- x [sapply (indices, function (v) v [[1]]), , drop = FALSE]
  }

  data.frame (counts = sapply (indices, length),
              ind    = I (indices),
              x)
}


## strat.kfold <- function (x, cprobe, cclass, nset, probs = c (1, .5), n = NULL){

##   ip <- 2 + (1 : length (cprobe)) # col index into n
##   ic <- (2 + length (ip)) + (1 : length (cclass)) # col index into n

##   if (is.null (n)){
##     n <- count.rows (x[,cprobe])
##     n <- cbind (n, 
##                 t (sapply (n$ind,
##                            function (x, y){apply (y[x,], 2, sum)},
##                            x[, cclass]))
##                 )

##     n [, ic] <- t (apply (n[, ic], 1, function (x) x / sum (x)))
##   }

##   rest <- sequence (nrow (n))
##   smpl <- integer (0)

##   for (p in probs){
##     for (c in ic [sample (length (ic))]){
##       dummy <- which (n[rest, c] >= p)
##       dummy <- dummy [sample (length (dummy))]
##       smpl <- c (smpl, rest [dummy])
##       if (length (dummy) > 0)
##         rest <- rest [-dummy]
##       if (length (rest) == 0)
##         break
##     }
##   }
##   if (length (rest) > 0)
##     smpl <- c (smpl, rest[sample (length (rest))]) 

##   n$testset <- rep (1 : nset, length.out = nrow (n))[order (smpl)]

##   n
## }

ls.cb <- function (nice = TRUE, order.size = TRUE){
  vars <- ls (parent.frame()) 
  vars <- data.frame (name = vars [! sapply (vars, function (v) is.function (get (v)))])

  vars$size <- sapply (vars$name, function (v) object.size (get (as.character(v) )))

  if (order.size)
    vars <- vars[order (vars$size),] 

  if (nice){
    vars$unit <- "B"
    vars$unit[vars$size > 1048576] <- "MB"
    vars$size[vars$size > 1048576] <- vars$size[vars$size >  1048576] /  1048576
    vars$unit[vars$size > 1024] <- "kB"
    vars$size[vars$size > 1024] <- vars$size[vars$size > 1024] / 1024

    vars$size <- round (vars$size, digits = 1) 
  }  
  vars
} 


my.extract <- function (x, head, tail, ...){
  if (is.null (dim (x)))
    dim <- length (x)
  else
    dim <- dim (x)

  stopifnot (length (head) == length (dim))
  stopifnot (length (tail) == length (dim))

  dots <- list (x = x) 
  
  for (d in seq_along (dim)){
    dots [[d + 1]] <- seq_len (dim [d]) 
    if (head[d] != 0)
      dots [[d + 1]] <- head (dots [[d + 1]], head [d])
    if (tail [d] != 0)
      dots [[d + 1]] <- tail (dots [[d + 1]], head [d]) 
  }

  dots <- c (dots, ...) 
  
  do.call ("[", dots) 
      
} 


