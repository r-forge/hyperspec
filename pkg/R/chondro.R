tmp <- tcrossprod (.scores, .loadings)
tmp <- tmp + rep (.center, each = nrow (.scores))

chondro <- new ("hyperSpec", spc = tmp, wavelength = .wl,
                data = .extra, labels = .labels,
                log = list (long = "example data chondro reconstructed from 10 principal components."))


