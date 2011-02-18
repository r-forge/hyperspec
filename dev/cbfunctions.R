
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

mark.dendrogram <- function (dendrogram, clusters,
                             col = matlab.dark.palette (length (unique (clusters))),
                             y = -3, pch = "|", ...)
  points (seq_along (dendrogram$order), rep (y, length (dendrogram$order)),
          col = col [clusters [dendrogram$order]], pch = pch, ...)

mark.dendrogram.r <- function (dendrogram, clusters,
                             col = matlab.dark.palette (nlevels (clusters)),
                               y1 = -3, y2 = -4, border = NA, ...){
  if (! is.factor (clusters))
    clusters <- as.factor (clusters)
  n <- length (dendrogram$order)
  rect (seq_len (n) - 0.5, rep (y1, n), seq_len (n) + 0.5, rep (y2, n),
          col = col [clusters [dendrogram$order]], border = border, ...)
}

abbr <- function (x, first = 3, last = 1){
  cln <- colnames (x)
  if (is.null (cln)) cln <- seq_len (ncol (x))
  cln <- paste (",", cln, sep = "")
  
  rwn <- rownames (x)
  if (is.null (rwn)) rwn <- seq_len (nrow (x))
  rwn <- paste (rwn, ",", sep = "")
  
  x <- format (x)

  dot <- format ('.', justify = "centre", width = nchar (x [1]))

  if (nrow (x) > first + last) {
    o <- min (nrow (x), first)
    u <- min (nrow (x) - first, last)

    x <- rbind (head (x, o), dot, tail (x, u))
    rownames (x) <- c (head (rwn, o), "", tail (rwn, u))
  } else {
    rownames (x) <- rwn
  }
  x <- t (x)

  if (ncol (x) > first + last) {
    f <- min (ncol (x), first)
    l <- min (ncol (x) - first, last)

    x <- rbind (head (x, f), dot, tail (x, l))
    rownames (x) <- c (head (cln, f), "", tail (cln, l))
  } else {
    rownames (x) <- cln
  }

  x <- t (x)

  x
}


str.matrix <- function (x) {
  cat (typeof (x), " ", class (x),
       " (", paste (dim (x), collapse = " x "), " = ", length (x), ")\n",
       sep = "")
  x <- abbr (x)

  cln <- format (colnames (x), justify = "centre", width = nchar (x [1]))
  rwn <- format (rownames (x))

  if (nchar (cln [1]) > nchar (x [1]))
    x <- format (x, width = nchar (cln [1]), justify = "right")
  cln <- c (format ("", width = nchar (rwn [1])), cln)

  cat (cln, "\n", sep = '  ')
  for (i in 1 : nrow (x))
    cat (rwn [i], x [i,], "\n", sep = '  ')

  invisible ()
}




