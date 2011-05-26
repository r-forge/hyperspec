.make.chondro <- function (){
#  load (system.file ("extdata/chondro-internal.rda", package = "hyperSpec"))
  
  new ("hyperSpec",
       spc =  (tcrossprod (.chondro.scores, .chondro.loadings) +
               rep (.chondro.center, each = nrow (.chondro.scores))),
       wavelength = .chondro.wl,
       data = .chondro.extra, labels = .chondro.labels,
       log = list (long = "example data chondro reconstructed from 10 principal components."))
}

delayedAssign ("chondro", .make.chondro ())

