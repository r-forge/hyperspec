setMethod ("length", "hyperSpec", function (x) {
  validObject (x)
  nrow (x@data)
})


