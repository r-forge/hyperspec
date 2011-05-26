###-----------------------------------------------------------------------------
###
### rbind2
###
###

setMethod("rbind2",
          signature(x = "hyperSpec", y = "hyperSpec"),
          function (x, y) {
            validObject (x)
            validObject (y)

            if (! isTRUE (all.equal (x@wavelength, y@wavelength)))
              stop ("The wavelengths of the objects differ.\n",
                    "If they are not ordered, try 'orderwl'.")

            x@data <- rbind (x@data, y@data)

            .logentry (x, short = "rbind2", long = list (y = as.character (y)))
          }
          )

setMethod ("rbind2", signature (x = "hyperSpec", y = "missing"), function (x, y) x)

