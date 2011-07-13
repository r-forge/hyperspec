
read.spe <- function (filename){
  f <- readBin (filename, "raw", file.info (filename)$size, 1)

  xdim <- readBin (f [43:44], "integer", 1, 2, signed = FALSE) # offset: +1 R starts indexing with 1
  ydim <- readBin (f [657:658], "integer", 1, 2, signed = FALSE)  

  datatype <- f [109] 

  nframes <- readBin (f [1447 : 1451], "integer", 1, 4, signed = FALSE)

  n <- xdim * ydim * nframes 
  spc <- switch (as.character (datatype),
                 "00" = readBin (f [4101 : length (f)], "double",  n, 4),
                 "01" = readBin (f [4101 : length (f)], "integer", n, 4, signed = TRUE),
                 "02" = readBin (f [4101 : length (f)], "integer", n, 2, signed = TRUE),
                 "03" = readBin (f [4101 : length (f)], "integer", n, 2, signed = FALSE),
                 default = stop ("unknown data type") 
                 )

  spc <- matrix (spc, byrow = TRUE, nrow = nframes)

  new ("hyperSpec",
       data = data.frame (frame = rep (seq_len (nframes), each = ydim)),
       spc = spc)
}
