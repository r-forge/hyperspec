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
    append <- TRUE
  }

  write.table (X, file, append = append, quote = quote, sep = sep,
               row.names = FALSE, col.names = FALSE, ...)
}
