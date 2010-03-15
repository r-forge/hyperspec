
###-----------------------------------------------------------------------------
###
###  as.character
###
###

setMethod (as.character, "hyperSpec", function (x,
                                                digits = getOption ("digits"),
                                                range = TRUE,
                                                max.print = 5,
                                                shorten.to = c(2,1),
                                                log = TRUE){
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

  chr <- c (chr, .paste.row (x@wavelength, x@label$.wavelength, "wavelength",
                             ins = 0, val = TRUE, range = FALSE,
                             shorten.to = shorten.to, max.print = max.print))
            
  n.cols <- ncol (x@data)

  chr <- c(chr, paste ("data: ", " (", nrow(x@data), " rows x ", n.cols,
                       " columns)", sep = ""))

  if (n.cols > 0)
    for (n in names (x@data))
      chr <- c(chr, .paste.row (x@data[[n]], x@label[[n]], n, ins = 3,
                                i = match (n, names (x@data)),
                                val = TRUE, range = range,
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

