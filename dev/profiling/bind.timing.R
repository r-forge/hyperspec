library ("microbenchmark")

microbenchmark (rbind = do.call (rbind, list),
                collapse = collapse (list))

library ("profr")

timing <- profr (bg <- collapse (bg))

timing <- profr (bg <- collapse (bg))
X11 ()
plot (timing)
