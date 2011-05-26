###-----------------------------------------------------------------------------
###
### wc - word count
###
###

wc <- function (file, flags = c("lines", "words", "bytes")){
  if (length (system ("wc --help", intern = TRUE)) == 0)
    return (NULL)

  wc <- paste ("wc", paste ("--", flags, sep = "", collapse = ", "), file)
  wc <- read.table(pipe (wc))
  colnames (wc) <- c(flags, "file")
  wc
}

