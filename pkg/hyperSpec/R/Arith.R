###-----------------------------------------------------------------------------
###
###  Arith
###
###

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)
             if (.Generic %in% c ("*", "^", "%%", "%/%", "/"))
               warning (paste ("Do you really want to use", .Generic, "on 2 hyperSpec objects?"))
             e1 [[]] <- callGeneric (e1[[]], e2[[]])
             .logentry (e1, short = .Generic, long = as.character (e2))
           }
           )

.arithx <- function (e1, e2){
  validObject (e1)
  if (missing (e2)){
    e1  [[]] <- callGeneric (e1 [[]])
    .logentry (e1, short = .Generic, long = list ())
  } else {
    e1  [[]] <- callGeneric (e1 [[]], e2)
    .logentry (e1, short = .Generic,
               long = list (e2 = .paste.row (e2, val = TRUE)))
  }
}

.arithy <- function (e1, e2){
  validObject (e2)
  e2  [[]] <- callGeneric (e1, e2 [[]])
  .logentry (e2, short = .Generic, long = list (e1 = .paste.row (e1, val = TRUE)))
}

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "numeric"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "matrix"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "missing"), .arithx)

setMethod ("Arith", signature (e1 = "numeric", e2 = "hyperSpec"), .arithy)
setMethod ("Arith", signature (e1 = "matrix", e2 = "hyperSpec"), .arithy)
