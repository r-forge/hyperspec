###-----------------------------------------------------------------------------
###
### .is.hy - checks whether the object is a hyperSpec object
###          to be used like validObject
###
### C. Beleites
###

.is.hy <- function (x){
  if (! is (x, "hyperSpec"))
    stop ("no hyperSpec object")

  TRUE
}

