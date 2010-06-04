df <- data.frame (x = 1 : 10,
                  y = I ( matrix (1 : 20, nrow = 10)),
                  z = -10 : -1)

tmp <- apply.df.with.matrix (df, 2, function (x) c (mean = mean (x), sd = sd (x)))
tmp
str (tmp)

library (plyr)
library (hyperSpec)

repdf <- df[, sample (1:3, 100, replace = TRUE)]
sum (grepl ("y", colnames (repdf)))

system.time (tmp <- apply (repdf, 2, mean))
system.time (tmp <- apply.df.with.matrix (repdf, 2, mean))

## check matrix with 0 columns
df <- data.frame (x = 1 : 10,
                  y = I ( matrix (nrow = 10, ncol = 0)),
                  z = -10 : -1)

tmp <- apply.df.with.matrix (df, 2, function (x) c (mean = mean (x), sd = sd (x)))
ncol (tmp)
ncol (tmp [, 2])


## function that leads to a list being returned
tmp <- apply.df.with.matrix (df, 2, function (x) which (x > 3))
