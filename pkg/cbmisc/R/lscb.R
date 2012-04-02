##' Detailed listing of objects
##'
##' @param names names of the object to list
##' @param sort column to sort
##' @return data.frame with columns giving name, class, length, dim and size of the objects.
##' @author Claudia Beleites
##' @export
lscb <- function (names = ls (envir = parent.frame ()), sort = "size"){
  size <- sapply (names, function (x) (object.size (get (x))))
  if (sort == "size")
    order <- order (size)

  exp <- floor (log (size, 1024))
  size <- size / 1024^exp
  units <- c (" B", "kB", "MB", "GB", "TB") [exp + 1]
  
  res <- data.frame (name = names,
              class = sapply (names, function (x) class (get (x))),
              length = sapply (names, function (x) length (get (x))),
              dim = sapply (names, function (x) paste (dim (get (x)), collapse = " x ")),
              size = sprintf ("%0.0f %s", round (size), units)
              )
    
  rownames (res) <- NULL

  if (sort != "size")
    order <- order (res [[sort]])
  
  res [order,]
}

