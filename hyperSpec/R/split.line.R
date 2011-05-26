###-----------------------------------------------------------------------------
###
### split.line - split line into list of key-value pairs
###
###

split.line <- function (x, separator, trim.blank = TRUE) {
  tmp <- regexpr (separator, x)
  #if (length (tmp) == 1 && tmp [[1]] == -1)
  #  warning ("line without separator", separator)

  key   <- substr (x, 1, tmp - 1)
  value <- substr (x, tmp + 1, nchar (x))

  if (trim.blank){
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    key <- sub (blank.pattern, "\\1", key)
    value <- sub (blank.pattern, "\\1", value)
  }

  value <- as.list (value)
  names (value) <- key

  value
}


