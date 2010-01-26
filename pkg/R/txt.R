###-----------------------------------------------------------------------------
###
###  read.txt.long: import measurements from .txt file
###
###  Format:
###  (y x) wl int
###

read.txt.long <- function (file = stop ("filename is required"),
                           cols = list (
                             .wavelength = expression (lambda / nm),
                             spc = "I / a.u."),
                           header = TRUE,
                           ...){
  txtfile <- read.table (file = file, header = header, ...)

  if (header){
    cln <- match (colnames (txtfile), names (cols))
    cln <- cols[cln]
    names (cln) <- colnames (txtfile)
    cols <- cln
    rm (cln)
  } else {
    if (ncol (txtfile) != length (cols)){
      warning (paste ("cols does not correspond to the columns in", file,
                      ". Guessing remaining columns."))
      cols <- c (character (ncol (txtfile) - 2), cols)
    }
  }


  if (is.na (match ("spc", names (cols))))
    stop ("cols$spc must exist.")

  wavelength <- match (".wavelength", names (cols))
  if (is.na (wavelength))
    stop ("cols$.wavelength must exist.")

  colnames (txtfile) <- names (cols)

  ## wavelength axis
  wavelength <- as.numeric (levels (as.factor (txtfile$.wavelength)))
  
  spc <- as.matrix (unstack (txtfile, form = spc ~ .wavelength))
  if ((nrow (spc)  == length (wavelength)) & (ncol (spc) != length (wavelength)))
    spc <- t (spc)

  colnames (spc) <- levels (txtfile$.wavelength)

  txtfile <- txtfile [txtfile$.wavelength == txtfile$.wavelength[1], ]
  txtfile$.wavelength <- NULL
  txtfile$spc <- I (spc)

  new ("hyperSpec",
       wavelength = wavelength,
       data = txtfile,
       label = cols,
       log = list (
         short = "read.txt.long",
         long = list (file = file, cols = I (cols), ...)
         )
       )
}

###-----------------------------------------------------------------------------
###
###  read.txt.wide
###
###  Format:
###  x y ... int (wl1)  int (wl2) ... int (wl p) z ...
###
read.txt.wide <- function (file = stop ("filename is required"),
                           cols = list (
                             spc = "I / a.u.",
                             .wavelength = expression (lambda / nm)),
                           check.names = FALSE,
                           ...){
  txtfile <- read.table (file = file, ..., check.names = FALSE)

  .wavelength <- match (".wavelength", names (cols))
  if (is.na (.wavelength))
    cols <- as.list (c (cols, .wavelength = expression (lambda / nm)))
  else
    if (.wavelength != length (cols))   # .wavelength should be at the end of cols
      cols <- cols [c (seq_along (cols)[-.wavelength], .wavelength)]

  ## columns containing the spectra
  spc <- match ("spc", names (cols))
  if (is.na (spc))
    stop ("cols$spc must exist.")

  spc <- 0 : (ncol (txtfile) - length (cols) + 1) + spc

  spc.data <- as.matrix (txtfile[, spc])
  txtfile$spc <- I (spc.data)
  txtfile <- txtfile [, -spc, drop = FALSE]


  new ("hyperSpec",
       data = txtfile,
       label = cols,
       log = list (
         short = "read.txt.long",
         long = list (file = file, cols = I (cols), ...)
         )
       )
}

###-----------------------------------------------------------------------------
###
### write.txt.wide
###
###

write.txt.wide <- function (object,
                            file = stop ("filename required"),
                            cols = NULL,
                            quote = FALSE, sep = "\t",
                            row.names = FALSE,
                            col.names = TRUE,
                            header.lines = 1,   # 1 or 2 line header?
                            col.labels = if (header.lines == 1) FALSE else TRUE, # use labels instead of column names?
                            append = FALSE,
                            ...){
  validObject (object)

  if (! is.null (cols))
    object <- object [, cols]

  if (col.names){
    col.spc <- match ("spc", colnames (object@data))

    if (col.labels){
      cln <- match (colnames (object@data), names (object@label))
      cln[!is.na (cln)] <- object@label [cln[!is.na(cln)]]
      cln[is.na (cln)] <- colnames (object@data) [is.na(cln)]
      cln <- sapply (cln, as.character)
                                        #cln [-col.spc] <- object@label []
    } else {
      cln <- colnames (object@data)
    }

    i <- seq_along (cln)

    if (header.lines == 1){
      write.table (matrix (c(if (row.names) "" else NULL,
                             cln [i < col.spc],
                             object@wavelength,
                             cln [i > col.spc]
                             ), nrow = 1),
                   file = file, append = append, quote = quote, sep = sep,
                   row.names = FALSE, col.names = FALSE)
      append = TRUE
    } else if (header.lines == 2) {
      ## 1st line
      write.table (matrix (c (
                              if (row.names) "" else NULL,
                              cln [i < col.spc],
                              if (col.labels) cln [col.spc] else "",
                              rep ("", length (object@wavelength) - 1),
                              cln [i > col.spc]), nrow = 1),
                   file = file, append = append, quote = quote, sep = sep,
                   row.names = FALSE, col.names = FALSE)
      append = TRUE
      ## 2nd line
      write.table (matrix (c (if (row.names) (if (col.labels) as.character (object@label$.wavelength)
      else "wavelength")
      else NULL,
                              rep ("", sum (i < col.spc)),
                              object@wavelength,
                              rep ("", sum (i > col.spc))
                              ), nrow = 1),
                   file = file, append = append, quote, sep,
                   row.names = FALSE, col.names = FALSE)

    } else {
      stop ("Only 1 or 2 line headers supported.")
    }

  }

  write.table (object@data, file = file, append = append, quote = quote, sep = sep,
               row.names = row.names, col.names = FALSE, ...)
}

###-----------------------------------------------------------------------------
###
### write.txt.long
###
###

write.txt.long <- function (object,
                            file = stop ("filename required"),
                            order = c (".rownames", ".wavelength"),
                            na.last = TRUE, decreasing = FALSE,
                            quote = FALSE, sep = "\t",
                            row.names = FALSE,
                            cols = NULL,
                            col.names = TRUE,
                            col.labels = FALSE, # use labels instead of column names?
                            append = FALSE,
                            ...){
  validObject (object)

  col.spc <- match ("spc", colnames (object@data))

  X <- as.long.df (object, rownames = TRUE)

  if (!is.null (order)){
    if (is.character (order)) {
      tmp <- match (order, colnames (X))
      if (any (is.na (tmp)))
        stop ("write.txt.long: no such columns: ",
              paste (order [is.na (tmp)], collapse = ", "))
      order <- tmp
      }


    if (length (decreasing) < length (order))
      decreasing <- rep (decreasing, length.out = length (order))

    order.data <- as.list (X [, order, drop = FALSE])

    for (i in seq_along (order)){
      if (is.factor(order.data [[i]]))
        order.data [[i]] <- rank (order.data [[i]], na.last = na.last | is.na (na.last))

      if (decreasing [i])
        order.data [[i]] <- - order.data [[i]]
    }

    X <- X[do.call ("order",
                    c (order.data, na.last = na.last | is.na (na.last),	decreasing = FALSE)
                    ), ]
  }

  if (is.na (na.last))
    X <- X[! is.na (X$spc), ]

  if (!is.null (cols))
    X <- X [, cols, drop = FALSE]

  if (!row.names)
    X$.rownames <- NULL
  else
    cln [match (".rownames", cln)] <- "row"

  if (col.names){
    if (col.labels){
      cln <- match (colnames (X), names (object@label))
      cln[!is.na (cln)] <- object@label [cln[!is.na(cln)]]
      cln[is.na (cln)] <- colnames (X) [is.na(cln)]
      cln <- sapply (cln, as.character)
    } else {
      cln <- colnames (X)
    }

    write.table (matrix (cln, nrow = 1), file = file, append = append,
                 quote = quote, sep = sep, row.names = FALSE, col.names = FALSE)
    append = TRUE
  }

  write.table (X, file, append = append, quote = quote, sep = sep,
               row.names = FALSE, col.names = FALSE, ...)
}
