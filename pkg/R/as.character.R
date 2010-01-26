###-----------------------------------------------------------------------------
###
###  .paste.row
###
###

.paste.row <- function (x, label = "", name = "", ins = 0, i = NULL, val = FALSE, range = TRUE,
                        digits = getOption ("digits"), max.print = 5, shorten.to = c (2,1)){
  if (is.null (name))
    name <- ""

  if (is.null (label))
    label <- ""
  else
    label <- paste (as.character (label), collapse = " ")
  
  row.text <- ""

  if (val){
    if (is.list (x)){
      row.text <- paste ("", "columns/entries", paste (names (x), collapse = ", "))
    } else {
      if (range)
        val <- sort (unique (as.vector (x)))
      else
        val <- x
      if (length (val) > max.print)
        row.text <- c(
                   format (val [seq_len (shorten.to[1])], # was 1 :
                           digits = digits, trim = TRUE),
                   "...",
                   format (val [-seq_len (length (val) - shorten.to[2])], # was 1 :
                           digits = digits, trim = TRUE)
                   )
      else
        row.text <- format (val, digits = digits, trim = TRUE)
      row.text <- paste ("", if(range) "range", paste (row.text, collapse = " "),
                         if(any (is.na (x))) "+ NA", collapse = " ")

    }
  }
  row.text <- paste (paste (rep (" ", ins), collapse = ""),
                  if (!is.null (i)) paste ("(", i, ") ", sep =""),
                  name,
                  if (nchar (name) != 0) ": ",
                  label,
                  if (nchar (label) != 0) " ",
                  if ((nchar (row.text) > 0) | (nchar (label) + nchar (name) > 0) | !is.null (i))
                     "[",
                  paste (class (x), collapse = ", "),
                  " ",
                  if (! is.null (dim (x)))
                     paste (if (is.matrix (x) & all (class (x) != "matrix")) "matrix " else
                            if (is.array (x) & all (class (x) != "array") & all (class (x) != "matrix"))
                            "array ",
                            paste (dim (x) [-1], collapse = " x ")
                            , sep = ""),
                  #else if (length (x) > 1)
                  #length (x),
                  if ((nchar (row.text) > 0) | (nchar (label) + nchar (name) > 0) | !is.null (i))
                     "]",
                  row.text,
                  sep ="")
  row.text
}

###-----------------------------------------------------------------------------
###
###  as.character
###
###

setMethod (as.character, "hyperSpec", function (x,
                                                digits = getOption ("digits"),
                                                max.print = 5,
                                                shorten.to = c(2,1),
                                                log = TRUE){#, ...){


  ## input checking
  validObject (x)

  if (is.null (max.print))
    max.print <- getOption ("max.print")

  if ((length (max.print) != 1) | ! is.numeric (max.print))
    stop ("max.print needs to be a number")
  if ((length (shorten.to) < 1) |(length (shorten.to) > 2) | ! is.numeric (shorten.to))
    stop ("shorten.to needs to be a numeric vector with length 1 or 2")
  if (sum (shorten.to) > max.print)
    stop ("sum (shorten.to) > max.print: this does not make sense.")

  ## printing information
  chr <- c("hyperSpec object",
           paste ("  ", nrow (x), "spectra"),
           paste ("  ", ncol (x), "data columns"),
           paste ("  ", nwl (x), "data points / spectrum")
           )

  chr <- c (chr, paste ("wavelength:",
                        .paste.row (x@wavelength, x@label$.wavelength, ins = 0, val = TRUE,
                                    range = FALSE, shorten.to = shorten.to, max.print = max.print),
                        collapse = " ")
            )

  n.cols <- ncol (x@data)

  chr <- c(chr, paste ("data: ",
                       " (", nrow(x@data), " rows x ", n.cols, " columns)", sep = ""))
  if (n.cols > 0)
    for (n in names (x@data))
      chr <- c(chr, .paste.row (x@data[[n]], x@label[[n]], n, ins = 3,
                                i = match (n, names (x@data)), val = TRUE,
                                shorten.to = shorten.to, max.print = max.print))

  if (log){
    chr <- c(chr, "log:")

    long <- lapply (as.character (x@log$long.description),
                    function (x, max.print, shorten.to, desc = TRUE){
                      if (nchar (x) > max.print)
                        paste (substr (x, 1, max.print), "...", sep = "")
                      else
                        x
                    },
                    max.print, shorten.to, desc = FALSE)
    width = c (4 + floor (log10 (nrow (x@log))),
      max (sapply (c("short", as.character (x@log$short.description)), nchar)),
      max (sapply (c("long", long), nchar)),
      max (sapply (c("date", as.character (x@log$date)), nchar)),
      max (sapply (c("user", as.character (x@log$user)), nchar))
      )

    chr <- c (chr, paste (paste (rep (" ", width [1]), collapse = ""),
                          format ("short", justify = "right", width = width [2]),
                          format ("long", justify = "right", width = width [3]),
                          format ("date", justify = "right", width = width [4]),
                          format ("user", justify = "right", width = width [5]),
                          sep = "   ")
              )

    for (i in seq_len (nrow (x@log)))
      chr <- c (chr, paste (format (i, justify = "right", width = width [1]),
                            format (as.character (x@log[i, 1]), justify = "right", width = width [2]),
                            format (long [i], justify = "right", width = width [3]),
                            format (as.character (x@log[i, 3]), justify = "right", width = width [4]),
                            format (as.character (x@log[i, 4]), justify = "right", width = width [5]),
                            sep = "   ")
                )
  }
  chr
})

